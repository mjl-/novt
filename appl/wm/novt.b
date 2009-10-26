# terminal emulator, using a tk text widget
# emulates some of vt220, and some ansi/ecma color codes.  not all of it.
# the text widget can grow very large.  the last 24 lines are the terminal window.
# each line is always 80 chars wide, followed by a newline.

implement Novt;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
	draw: Draw;
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "sh.m";
	sh: Sh;
include "string.m";
	str: String;
include "tk.m";
	tk: Tk;
include "tkclient.m";
	tkclient: Tkclient;
include "keyboard.m";
	kb: Keyboard;
include "util0.m";
	util: Util0;
	pid, min, max, warn, killgrp: import util;

Novt: module {
	init:	fn(ctxt: ref Draw->Context, argv: list of string);
};


dflag: int;
command: list of string;

echo := 0;
escs := 1;
nlines: int;
bell: int;
slow := 0;
cursor := 1;

State: adt {
	y,
	x,
	insert,			# or replace
	autowrap:	int;
	bold,
	underline,
	blink,
	reverse,
	fg,
	bg:		int;
};

Black, Red, Green, Yellow, Blue, Magenta, Cyan, White: con iota;
# normal, bold
colors := array[] of {
("#000000", "#000000"),
("#dd0000", "#ff0000"),
("#00dd00", "#00ff00"),
("#ffdd00", "#ffff00"),
("#0000dd", "#0000ff"),
("#dd00dd", "#ff00ff"),
("#00dddd", "#00ffff"),
("#dddddd", "#ffffff"),
};

Fg: con White;
Bg: con Black;
st := State (0, 0, 0, 1, 0, 0, 0, 0, Fg, Bg);
savedstate: State;


tocmd: ref Sys->FD;
inc: chan of (array of byte, string);
t: ref Tk->Toplevel;
wmctl: chan of string;

cc: chan of int; # chars to terminal code parser
syncc: chan of int;

tkcmds0 := array[] of {
"frame .c",
"button .c.exit -text exit -command {send cmd exit}",
"label .c.bell0 -fg red -text '     ",
"label .c.bell1 -fg red -text '     ",
"button .c.paste -text paste -command {send cmd paste}",
"button .c.echo -text echo -command {send cmd echo}",
"button .c.noecho -text noecho -command {send cmd noecho}",
"button .c.r80x24 -text 80x24 -command {send cmd 80x24}",
"button .c.r132x24 -text 132x24 -command {send cmd 132x24}",
"button .c.debug -text debug -command {send cmd debug}",
"button .c.nodebug -text nodebug -command {send cmd nodebug}",
"button .c.escs -text escs -command {send cmd escs}",
"button .c.noescs -text noescs -command {send cmd noescs}",
"button .c.x -text x -command {send cmd x}",
"pack .c.exit .c.bell0 .c.bell1 .c.paste .c.echo .c.noecho .c.r80x24 .c.r132x24 .c.debug .c.nodebug .c.escs .c.noescs .c.x -side left",
"label .error -fg red",
"frame .f",
"scrollbar .f.scroll -command {.t yview}",
"pack .f.scroll -in .f -side left -fill y",
"text .t -width 80w -height 24h -fg #dddddd -bg black -yscrollcommand {.f.scroll set}",
"bind .t <Key> {send key %K}",
"bind .t <ButtonRelease-2> {send cmd cut}",
"bind .t <Button-3> {send cmd paste}",
"pack .t -in .f -side right",
"pack .c .error .f",
"focus .t",
};

context: ref Draw->Context;
init(ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	if(ctxt == nil)
		fail("no window context");
	draw = load Draw Draw->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	sh = load Sh Sh->PATH;
	sh->initialise();
	str = load String String->PATH;
	tk = load Tk Tk->PATH;
	tkclient = load Tkclient Tkclient->PATH;
	util = load Util0 Util0->PATH;
	util->init();

	if(ctxt == nil)
		ctxt = tkclient->makedrawcontext();
	if(ctxt == nil)
		fail("no window context");
	context = ctxt;

	sys->pctl(Sys->NEWPGRP, nil);

	arg->init(args);
	arg->setusage(arg->progname()+" [-de] [command ...]");
	while((ch := arg->opt()) != 0)
		case ch {
		'd' =>	dflag++;
		'e' =>	echo++;
		* =>	arg->usage();
		}
	args = arg->argv();
	if(args == nil)
		command = list of {"sh", "-i"};
	else
		command = args;

	sys->pctl(Sys->NEWPGRP, nil);
	tkclient->init();
	(t, wmctl) = tkclient->toplevel(ctxt, "", "novt", Tkclient->Appl);

	cc = chan of int;
	syncc = chan of int;

	inc = chan of (array of byte, string);
	ctrlc := chan of string;
	escc := chan of string;
	cmdc := chan of string;
	keyc := chan of string;
	tk->namechan(t, ctrlc, "ctrl");
	tk->namechan(t, escc, "esc");
	tk->namechan(t, cmdc, "cmd");
	tk->namechan(t, keyc, "key");
	tkcmds(tkcmds0);
	for(i := 'A'; i <= '_'; i++)
		if(i != '\\' && i != '[')
			tkcmd(sprint("bind .t <Control-%c> {send ctrl %c}", i, i));
	tkcmd("bind .t <Control-\\\\> {send ctrl \\\\}");
	tkcmd(sprint("bind .t <Control-?> {send ctrl %c}", 16r7F+16r40));  # note that it's utf-8
	# xxx @ and [
	tkcmd(sprint("bind .t <Key-%c> {send esc A}", kb->Up));
	tkcmd(sprint("bind .t <Key-%c> {send esc B}", kb->Down));
	tkcmd(sprint("bind .t <Key-%c> {send esc C}", kb->Right));
	tkcmd(sprint("bind .t <Key-%c> {send esc D}", kb->Left));
	tkcmd(".t tag configure ul -underline 1");
	tkcmd(".t tag configure st -overstrike 1");
	for(i = 0; i < len colors; i++) {
		tkcmd(sprint(".t tag configure f%d -foreground %s", i, colors[i].t0));
		tkcmd(sprint(".t tag configure f%db -foreground %s", i, colors[i].t1));
		tkcmd(sprint(".t tag configure b%d -background %s", i, colors[i].t0));
		tkcmd(sprint(".t tag configure b%db -background %s", i, colors[i].t1));
	}

	nlines = 24;
	empty();

	tkclient->onscreen(t, nil);
	tkclient->startinput(t, "kbd"::"ptr"::nil);

	fromcmd0, fromcmd1: ref Sys->FD;
	(tocmd, fromcmd0, fromcmd1) = run(command);
	spawn reader(fromcmd0);
	spawn reader(fromcmd1);
	fromcmd0 = fromcmd1 = nil;
	spawn parser();

	for(;;) alt {
	s := <-t.ctxt.kbd =>
		tk->keyboard(t, s);

	s := <-t.ctxt.ptr =>
		tk->pointer(t, *s);

	s := <-t.ctxt.ctl or
	s = <-t.wreq =>
		tkclient->wmctl(t, s);

	menu := <-wmctl =>
		case menu {
		"exit" =>
			killgrp(sys->pctl(0, nil));
			exit;
		* =>
			tkclient->wmctl(t, menu);
		}

	s := <-ctrlc =>
		c := s[0];
		c -= 16r40;
		if(ewrite(tocmd, d := sys->aprint("%c", c), len d) != len d) {
			seterror(sprint("write: %r"));
			continue;
		}
		if(echo)
			drawc(c);

	e := <-escc =>
		e = "\u001b["+e;
		d := array of byte e;
		if(ewrite(tocmd, d, len d) != len d) {
			seterror(sprint("write: %r"));
			continue;
		}

	cmd := <-cmdc =>
		say(sprint("cmd: %q", cmd));
		case cmd {
		"exit" =>
			killgrp(pid());
			return;
		"paste" =>
			s := tkclient->snarfget();
			d := array of byte s;
			if(ewrite(tocmd, d, len d) != len d) {
				seterror(sprint("write: %r"));
				continue;
			}
			if(echo)
				for(i = 0; i < len s; i++)
					drawc(s[i]);
		"cut" =>
			s := tkcmd(sprint(".t get sel.first sel.last"));
			tkclient->snarfput(s);

		"echo" =>	echo = 1;
		"noecho" =>	echo = 0;
		"debug" =>	dflag++;
		"nodebug" =>	dflag = 0;
		"escs" =>	escs = 1;
		"noescs" =>	escs = 0;
		"80x24" =>
			tkcmd(".t configure -width 80w -height 24h");
			tkcmd("update");
		"132x24" =>
			tkcmd(".t configure -width 132w -height 24h");
			tkcmd("update");
		"x" =>
			warn("state: "+statetext(st));
			warn("saved state: "+statetext(savedstate));
			warn(sprint("echo %d, escs %d, nlines %d, cursor %d, .t end %q", echo, escs, nlines, cursor, tkcmd(".t index end")));
			#slow = !slow;
			setcursor();
			tkcmd("update");
		* =>
			warn(sprint("unknown command %#q", cmd));
		}
		tkcmd("focus .t");

	k := <-keyc =>
		c := str->toint(k, 16).t0;
		d: array of byte;
		case c {
		kb->Up =>	d = esc("A");
		kb->Down => 	d = esc("B");
		kb->Right =>	d = esc("C");
		kb->Left =>	d = esc("D");
		* =>
			d = sys->aprint("%c", c);
		}
		if(ewrite(tocmd, d, len d) != len d) {
			seterror(sprint("write: %r"));
			continue;
		}
		if(echo)
			drawc(c);

	(buf, err) := <-inc =>
		if(err != nil) {
			seterror("read: "+err);
			continue;
		}
		if(buf == nil)
			tocmd = nil;
		else
			drawbuf(buf);
	}
}

seterror(s: string)
{
	tkcmd(sprint(".error configure -text '%s", s));
	tkcmd("update");
}

statetext(s: State): string
{
	return sprint("y %d, x %d, insert %d, autowrap %d, bold %d underline %d, blink %d, reverse %d, fg %d, bg %d", s.y, s.x, s.insert, s.autowrap, s.bold, s.underline, s.blink, s.reverse, s.fg, s.bg);
}

setcursor()
{
	if(cursor)
		tkcmd(sprint(".t mark set insert %d.%d", tkline(st.y), st.x));
}

tkline(i: int): int
{
	return nlines-24+1+i;
}

pos(): string
{
	return sprint("%d.%d", tkline(st.y), st.x);
}

esc(s: string): array of byte
{
	return sys->aprint("\u001b[%s", s);
}

empty()
{
	line := string array[80] of {* => byte ' '};
	for(i := 0; i < 24; i++) {
		l := tkline(i);
		tkcmd(sprint(".t insert %d.0 '%s\n", l, line));
		tktags(sprint("%d.0 %d.80", l, l));
	}
}

lineclear(s, e, line: int, attrs: int)
{
	if(dflag) say(sprint("lineclear, s %d, e %d, line %d", s, e, line));
	l := tkline(line);
	tkcmd(sprint(".t delete %d.%d %d.%d; .t insert %d.%d '%s", l, s, l, e, l, s, string array[e-s] of {* => byte ' '}));
	if(attrs)
		tktags(sprint("%d.%d %d.%d", l, s, l, e));
}

chardel(s, e, line: int)
{
	l := tkline(line);
	tkcmd(sprint(".t delete %d.%d %d.%d", l, s, l, e));
}

charins(s, line: int, ss: string)
{
	l := tkline(line);
	tkcmd(sprint(".t insert %d.%d '%s", l, s, ss));
	tktags(sprint("%d.%d", l, s));
}

reader(fd: ref Sys->FD)
{
	for(;;) {
		n := sys->read(fd, buf := array[1024] of byte, len buf);
		if(n < 0)
			inc <-= (nil, sprint("%r"));
		if(n == 0) {
			inc <-= (nil, nil);
			break;
		}
		inc <-= (buf[:n], nil);
	}
}

getc(sc: int): int
{
	if(slow)
		sys->sleep(100);
	c := <-cc;
	if(dflag) say(sprint("getc %c (%#x)", c, c));
	if(sc)
		sync();
	return c;
}

sync()
{
	syncc <-= 1;
}

getnum(c: int, s: string): (int, string, string)
{
	is := "";
	if(c >= 0)
		is[len is] = c;
	for(;;) {
		s[len s] = c = getc(0);
		if(c < '0' || c > '9')
			break;
		is[len is] = c;
		sync();
	}
	return (c, is, s);
}

parser()
{
	for(;;) {
		(oy, ox) := (st.y, st.x);
		parse();
		if(oy != st.y || ox != st.x)
			setcursor();
		tkcmd("update");
	}
}

parse()
{
	c := getc(0);
	if(dflag) say(sprint("c %#x, at %d,%d", c, st.y, st.x));

	if(c == 16r1b && escs) {
		sync();
		escparse();
		return;
	}

	case c {
	'\0' =>
		; # ignore
	16r7 =>
		tkcmd(sprint(".c.bell%d configure -text '     ", bell));
		bell = 1-bell;
		tkcmd(sprint(".c.bell%d configure -text 'bell!", bell));
	16r8 =>
		st.x = max(0, st.x-1);
	'\t' =>
		warn("vertical tab, not implemented");
	'\r' =>
		st.x = 0;
	'\n' =>
		newline();
	# '\f' =>
	# 16r7f =>
	* =>
		putchar(c);
	}
	sync();
}

tktag(tag, pos: string)
{
if(dflag) say(sprint("tag %q, pos %q", tag, pos));
	tkcmd(sprint(".t tag add %s %s", tag, pos));
}

tktags(p: string)
{
	if(st.underline)
		tktag("ul", p);
	if(!st.bold && !st.reverse && st.fg == Fg && st.bg == Bg)
		return;
	fg := string st.fg;
	bg := string st.bg;
	if(st.reverse)
		(fg, bg) = (bg, fg);
	if(st.bold) {
		fg += "b";
		bg += "b";
	}
	tktag("f"+fg, p);
	tktag("b"+bg, p);
}

lastchar: int;
putchar(c: int)
{
	if(st.insert)
		tkcmd(sprint(".t delete %d.%d; .t insert %s '%c", tkline(st.y), 80-1, pos(), c));
	else
		tkcmd(sprint(".t delete %s; .t insert %s '%c", pos(), pos(), c));
	tktags(pos());
	if(st.x == 79 && st.autowrap) {
		st.x = 0;
		newline();
	} else if(st.x < 79) {
		st.x++;
	}
	lastchar = c;
}

newline()
{
	if(st.y+1 >= 24) {
		line := string array[81] of {* => byte ' ', 80 => byte '\n'};
		tkcmd(sprint(".t insert %d.0 '%s", tkline(nlines), line));
		nlines++;
		st.y = 23;
		tkcmd(".t see end; .t see {end -24l}");
	} else
		st.y++;
}

escparse()
{
	c := getc(1);
	case c {
	# single char escapes
	'=' =>	; # application keypad
	'>' =>	; # numeric keypad
	'D' =>
		# index, one line down. xxx scroll
		st.y = min(23, st.y+1);
	'M' =>
		# reverse index, one line up.  xxx scroll
		st.y = max(0, st.y-1);
		st.x = 0;
	'E' =>
		# NEL, to start of next line
		st.y = min(23, st.y+1);
		st.x = 0;
	'7' =>
		savedstate = st;
	'8' =>
		st = savedstate;
	'H' =>
		warn(sprint("horizontal tab set"));
	'c' =>
		# reset to init
		warn("reset to initial state, not implemented");

	# two char escapes
	'(' or ')' or '*' or '+' or '0' or '3' =>
		s := "";
		s[0] = c;
		s[1] = getc(1);
		case s {
		"(A" =>	; # UK chars-g0
		")A" =>	; # UK graphic-g1
		"(B" =>	; # ASCII - g0
		")B" =>	; # ASCII - g1
		"*B" =>	; # ASCII - g2
		"+B" =>	; # ASCII - g3
		"(0" =>	; # DEC charset - g0
		")0" =>	; # DEC charset - g1
		"*0" =>	; # DEC charset - g2
		"+0" =>	; # DEC charset - g3
		"(<" =>	; # DEC supplement-g0
		")<" =>	; # DEC supplement-g1
		"*<" =>	; # DEC supplement-g2
		"+<" =>	; # DEC supplement-g3
		* =>
			say(sprint("unknown two-char escape %#q", s));
		}

	# variable char escapes
	'[' =>
		s := "[";
		q := 0;
		s[len s] = c = getc(0);
		args: list of int;
		case c {
		'?' or
		'0' to '9' =>
			if(c == '?') {
				sync();
				q = 1;
				s[len s] = c = getc(0);
				if(c < '0' || c > '9')
					break;
			}
			sync();
		nums:
			for(;;) {
				is: string;
				(c, is, s) = getnum(c, s);
				if(is == nil) {
					sync();
					warn(sprint("empty number, %#q", s));
					if(c == ';')
						continue;
					return;
				}
				args = int is::args;
				case c {
				';' =>
					c = -1;
					sync();
				* =>
					break nums;
				}
			}
		}
		a := l2aintrev(args);

		# still need a sync()
		case c {
		'!' =>
			sync();
			s[len s] = c = getc(0);
			case c {
			'p' =>
				# reset to powerup
				warn("power-up default states, not implemented");
				return sync();
			* =>
				warn(sprint("unknown multi-char escape0 %#q", s));
				return sync();
			}
		'm' =>
			i := 0;
			if(len a == 0)
				a = array[] of {0};
			v: int;
		mode:
			while(i < len a)
				case v = a[i++] {
				0 =>	st.bold = st.underline = st.blink = st.reverse = 0;
				1 =>	st.bold = 1;
				2 =>
					if(i+1 >= len a) {
						warn(sprint("'m 2' without parameter..."));
						break mode;
					}
					case v = a[i++] {
					2 =>	st.bold = 0;
					4 =>	st.underline = 0;
					5 =>	st.blink = 0;
					7 =>	st.reverse = 0;
					* =>	warn(sprint("unknown mode disable %d, ignoring", v));
					}
				# 3 italic
				4 =>	st.underline = 1;
				5 =>	st.blink = 1;
				7 =>	st.reverse = 1;
				10 =>	; # primary/normal font
				30 to 39 =>
					fgc := v-30;
					if(fgc < 8)
						st.fg = fgc;
					else
						st.fg = White;
				40 to 49 =>
					bgc := v-40;
					if(bgc < 8)
						st.bg = bgc;
					else
						st.bg = Black;
				* =>	warn(sprint("unknown mode %d, ignoring", v));
				}
		'h' or 'l' =>
			v := c == 'h';
			if(!q)
			for(i := 0; i < len a; i++)
				case a[i] {
				2 =>	warn(sprint("keyboard locked %d, not implemented", v));
				4 =>	st.insert = v;
				12 =>	warn(sprint("send,receive %d, not implemented", v));
				20 =>	warn(sprint("newline/linefeed %d, not implemented", v));
				* =>
					warn(sprint("mode %d, value %d, not implemented", a[i], v));
				}
			if(q)
			for(i = 0; i < len a; i++)
				case a[i] {
				1 =>	
					if(v)
						warn("cursor ansi, not implemented");
				2 =>	warn("switch to vt52 mode, not implemented");
				3 =>	if(v)
						warn("columns 132, not implemented");
				4 =>	if(v)
						warn("smooth scrolling, not implemented");
				5 =>	if(v)
						warn("reverse color screen, not implemented");
				6 =>	if(v)
						warn("user-selectable origin, not implemented");
				7 =>	st.autowrap = v;
				8 =>	warn(sprint("autorepeat %d, not implemented", v));
				18 =>	warn(sprint("print formfeed %d, not implemented", v));
				19 =>	warn(sprint("print extent full screen/scrolling, %d, not implemented", v));
				25 =>
					cursor = v;
					state := "disabled";
					if(cursor)
						state = "normal";
					tkcmd(sprint(".t configure -state %s", state));
				42 =>	warn(sprint("character set, national/multinational %d, not implemented", v));
				# 47, xterm mode to switch main/alternate buffer.  for saving/restoring normal terminal text between cursor apps.
				* =>
					warn(sprint("private/extended mode %d, value %d, not implemented", a[i], v));
				}
		'r' =>
			# scroll region, exactly two args
			if(len a != 2) {
				warn(sprint("set scroll region, not two args..."));
				break;
			}
			warn(sprint("set scroll region %d %d, not implemented", a[0], a[1]));
		'A' =>
			st.y = max(0, st.y-value(1, a)); # up
		'B' =>
			st.y = min(23, st.y+value(1, a)); # down
		'C' =>
			st.x = min(79, st.x+value(1, a)); # right
		'D' =>
			st.x = max(0, st.x-value(1, a)); # left
		'H' or 'f' =>
			ny := nx := 0;
			if(len a == 2) {
				ny = a[0]-1;
				nx = a[1]-1;
			}
			st.y = minmax(0, ny, 24-1);
			st.x = minmax(0, nx, 80-1);
			if(dflag) say(sprint("new x %d, y %d", st.x, st.y));

		'K' =>
			# erase line, to end, to begin, or whole
			case value(0, a) {
			0 =>	lineclear(st.x, 80, st.y, 1);
			1 =>	lineclear(0, st.x+1, st.y, 1);
			2 =>	lineclear(0, 80, st.y, 1);
			}
		'J' =>
			# erase screen, to end, to begin, or whole
			case value(0, a) {
			0 =>
				lineclear(st.x, 80, st.y, 1);
				for(i := st.y+1; i < 24; i++)
					lineclear(0, 80, i, 1);
			1 =>
				for(i := 0; i < st.y; i++)
					lineclear(0, 80, i, 1);
				lineclear(0, st.x+1, st.y, 1);
			2 =>
				for(i := 0; i < 24; i++)
					lineclear(0, 80, i, 1);
			}
		'X' =>
			# erase char
			n := 1;
			if(len a == 1)
				n = max(1, a[0]);
			n = min(80-st.x, n);
			chardel(st.x, st.x+n, st.y);
			l := tkline(st.y);
			tkcmd(sprint(".t delete %d.%d %d.%d; .t insert %d.%d '%s", l, st.x, l, st.x+n, l, st.x, string array[n] of {* => byte ' '}));
			
		'@' =>
			# insert chars
			n := 1;
			if(len a == 1)
				n = max(1, a[0]);
			n = min(80-st.x, n);
			chardel(st.x+n, 80, st.y);
			# no attributes
			tkcmd(sprint(".t insert %d.%d '%s", tkline(st.y), st.x, string array[n] of {* => byte ' '}));

		'P' =>
			# delete chars at cursor position, adding spaces at the end.  spaces have no attrs, moved chars keep theirs
			n := 1;
			if(len a == 1)
				n = max(1, a[0]);
			n = min(80-st.x, n);
			chardel(st.x, st.x+n, st.y);
			# no attributes
			tkcmd(sprint(".t insert %d.%d '%s", tkline(st.y), st.x, string array[n] of {* => byte ' '}));

		'L' =>
			# insert line
			st.x = 0;
			n := 1;
			if(len a == 1)
				n = max(1, a[0]);
			n = min(24-st.y, n);
			line := string array[81] of {* => byte ' ', 80 => byte '\n'};
			for(i := 0; i < n; i++)
				tkcmd(sprint(".t delete %d.0 .%d.0", tkline(24-1-n), tkline(24)));
			for(i = 0; i < n; i++)
				tkcmd(sprint(".t insert %d.0 '%s", tkline(24-1-n+i), line));
		'M' =>
			# delete line
			st.x = 0;
			n := 1;
			if(len a == 1)
				n = max(1, a[0]);
			n = min(24-st.y, n);
			for(i := 0; i < n; i++)
				lineclear(0, 80, st.y+i, 0);
		'g' =>
			case v := value(0, a) {
			0 =>	warn("clear horizontal tab stop at cursor position, not implemented");
			1 =>	warn("clear line tabulation stop at the active line, not implemented");
			2 =>	warn("clear all character tabulation stops in the active line");
			3 =>	warn("clear all horizontal tab stops, not implemented");
			4 =>	warn("clear all line tabulation stops, not implemented");
			5 =>	warn("clear all tabulation stops, not implemented");
			* =>	warn(sprint("unsupported [g%d", v));
			}
		'"' =>
			sync();
			c = getc(0);
			case c {
			'p' =>
				warn(sprint("compatibility level, not implemented"));
			'q' =>
				# for setting character erasability
				warn(sprint("set character attributes, not implemented"));
			* =>
				warn(sprint("xxx"));
				return sync();
			}
		'd' =>
			# ansi, set line position
			v := value(1, a);
			st.y = minmax(0, v-1, 23);
		'G' =>
			# ansi, set column position
			v := value(1, a);
			st.x = minmax(0, v-1, 80);
		'b' =>
			# ansi, repeat preceding (in data stream) char n times
			v := value(1, a);
			for(i := 0; i < v; i++)
				putchar(lastchar);
		'c' =>
			d := sys->aprint("\u001b?62;6c");
			if(ewrite(tocmd, d, len d) != len d) {
				seterror(sprint("write: %r"));
				return sync();
			}
		'E' =>
			# ansi CNL, to start of next line
			n := value(1, a);
			st.y = min(23, st.y+n);
			st.x = 0;
		'F' =>
			# ansi CPL, to start of preceding line
			n := value(1, a);
			st.y = max(0, st.y-n);
			st.x = 0;
		'n' =>
			warn(sprint("get device status, not implemented"));
		* =>
			warn(sprint("unknown multi-char escape3 %#q", s));
			return sync();
		}
		sync();

	* =>
		say(sprint("unknown escape %c (%#x)", c, c));
	}
}

value(v: int, a: array of int): int
{
	if(len a == 0)
		return v;
	return a[0];
}

drawc(c: int)
{
	cc <-= c;
	<-syncc;
}

leftover: array of byte;
drawbuf(d: array of byte)
{
	if(len leftover != 0) {
		nd := array[len leftover+len d] of byte;
		nd[:] = leftover;
		nd[len leftover:] = d;
		d = nd;
	}
	n := sys->utfbytes(d, len d);
	leftover = nil;
	if(n < len d)
		leftover = d[n:];

	s := string d[:n];
	for(i := 0; i < len s; i++)
		drawc(s[i]);
}

tkcmd(s: string): string
{
	r := tk->cmd(t, s);
	if(r != nil && r[0] == '!')
		warn(sprint("tkcmd: %q: %s", s, r));
	return r;
}

tkcmds(a: array of string)
{
	for(i := 0; i < len a; i++)
		tkcmd(a[i]);
}

run(argv: list of string): (ref Sys->FD, ref Sys->FD, ref Sys->FD)
{
	if(sys->pipe(fd0 := array[2] of ref Sys->FD) != 0)
		fail(sprint("pipe: %r"));
	if(sys->pipe(fd1 := array[2] of ref Sys->FD) != 0)
		fail(sprint("pipe: %r"));
	if(sys->pipe(fd2 := array[2] of ref Sys->FD) != 0)
		fail(sprint("pipe: %r"));
	spawn run0(argv, fd0[1], fd1[0], fd2[0]);
	return (fd0[0], fd1[1], fd2[1]);
}

run0(argv: list of string, fd0, fd1, fd2: ref Sys->FD)
{
	sys->pctl(Sys->NEWFD, list of {fd0.fd, fd1.fd, fd2.fd});
	sys->dup(fd0.fd, 0);
	sys->dup(fd1.fd, 1);
	sys->dup(fd2.fd, 2);
	err := sh->run(context, argv);
	if(err != nil)
		warn(err);
}

ewrite(fd: ref Sys->FD, d: array of byte, n: int): int
{
	{
		return sys->write(fd, d, n);
	} exception ex {
	"write on closed pipe" =>
		sys->werrstr(ex);
		return -1;
	}
}

l2aintrev(l: list of int): array of int
{
	a := array[len l] of int;
	i := len a-1;
	for(; l != nil; l = tl l)
		a[i--] = hd l;
	return a;
}

minmax(a, b, c: int): int
{
	return max(a, min(b, c));
}

say(s: string)
{
	if(dflag)
		warn(s);
}

fail(s: string)
{
	warn(s);
	killgrp(pid());
	raise "fail:"+s;
}

# terminal emulator, using a tk text widget
# emulates some of ecma-48, not all of it.
# the text widget can grow very large.  the last lines are the terminal window.
# each line is always filled with characters (spaces for filling), followed by a newline.
# we start with 24 lines with 80 columns, but the window can be resized.

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

Novt: module {
	init:	fn(ctxt: ref Draw->Context, argv: list of string);
};


Unimpl, Esc, Chars, Tkdraw: con iota+1;
dflag: int;
command: list of string;

echo := 0;
escs := 1;
columns: int;
rows: int;
nlines: int;
bell: int;
cursor := 1;
error: int;

State: adt {
	y,
	x,
	autowrap,
	bold,
	underline,
	reverse,
	fg,
	bg:		int;
};

Black, Red, Green, Yellow, Blue, Magenta, Cyan, White: con iota;
# normal, bold
fgcolors := array[] of {
("#000", "#222"),
("#d00", "#f00"),
("#0d0", "#0f0"),
("#fd0", "#ff0"),
("#22d", "#22f"),
("#d0d", "#f0f"),
("#0dd", "#0ff"),
("#ddd", "#fff"),
};
bgcolors := array[] of {
("#000", "#000"),
("#d00", "#f00"),
("#0d0", "#0f0"),
("#fd0", "#ff0"),
("#22d", "#22f"),
("#d0d", "#f0f"),
("#0dd", "#0ff"),
("#ddd", "#fff"),
};

Fg: con White;
Bg: con Black;
nilstate: con State (0, 0, 1, 0, 0, 0, Fg, Bg);
st := nilstate;

tocmd: ref Sys->FD;
inc: chan of (array of byte, string);
t: ref Tk->Toplevel;
wmctl: chan of string;

cc: chan of int; # chars to terminal code parser
syncc: chan of int;

fontwidth,
fontheight: int;

Msnarf, Mpaste, Mclear, Mdim, Mbreak, Mescs, Mecho, Mdebug, Mnodebug, Mredial, Mx: con iota;
tkcmds0 := array[] of {
"menu .m",
".m add command -label snarf	-command {send cmd snarf}",
".m add command -label paste	-command {send cmd paste}",
".m add command -label clear	-command {send cmd clear}",
".m add command -label dim	-command {send cmd dim}",
".m add command -label break	-command {send cmd break}",
".m add command -label noescs	-command {send cmd escs}",
".m add command -label echo	-command {send cmd echo}",
".m add command -label debug	-command {send cmd debug}",
".m add command -label nodebug	-command {send cmd nodebug}",
".m add command -label redial	-command {send cmd redial}",
".m add command -label x	-command {send cmd x}",

"frame .f",
"scrollbar .f.scroll -command {.t yview}",
"pack .f.scroll -in .f -side left -fill y",
"text .t -width 80w -height 24h -fg #dddddd -bg black -yscrollcommand {.f.scroll set}",
"bind .t <Key> {send key %K}",
"bind .t <ButtonPress-2> {.m post %X %Y}",
"pack .t -in .f -side right -fill both -expand 1",
"pack .f -fill both -expand 1",
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

	if(ctxt == nil)
		ctxt = tkclient->makedrawcontext();
	if(ctxt == nil)
		fail("no window context");
	context = ctxt;

	sys->pctl(Sys->NEWPGRP|Sys->FORKNS, nil);

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

	tkclient->init();
	(t, wmctl) = tkclient->toplevel(ctxt, "", "novt", Tkclient->Appl);

	ctrlc := chan of string;
	escc := chan of string;
	cmdc := chan of string;
	keyc := chan of string;
	tk->namechan(t, ctrlc, "ctrl");
	tk->namechan(t, escc, "esc");
	tk->namechan(t, cmdc, "cmd");
	tk->namechan(t, keyc, "key");
	tkcmds(tkcmds0);
	if(echo)
		tkcmd(sprint(".m entryconfigure %d -label 'noecho", Mecho));
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
	tkcmd(sprint("bind .t <Key-%c> {send esc 2~}", kb->Ins));
	#tkcmd(sprint("bind .t <Key-%c> {send esc ?}", kb->Del));
	tkcmd(sprint("bind .t <Key-%c> {send esc H}", kb->Home));
	tkcmd(sprint("bind .t <Key-%c> {send esc 4~}", kb->End));
	tkcmd(sprint("bind .t <Key-%c> {send esc 5~}", kb->Pgup));
	tkcmd(sprint("bind .t <Key-%c> {send esc  6~}", kb->Pgdown));
	tkcmd(".t tag configure ul -underline 1");
	for(i = 0; i < len fgcolors; i++) {
		tkcmd(sprint(".t tag configure f%d -foreground %s", i, fgcolors[i].t0));
		tkcmd(sprint(".t tag configure f%db -foreground %s", i, fgcolors[i].t1));
		tkcmd(sprint(".t tag configure b%d -background %s", i, bgcolors[i].t0));
		tkcmd(sprint(".t tag configure b%db -background %s", i, bgcolors[i].t1));
	}

	columns = 80;
	rows = 24;
	nlines = 24;
	line := mkline();
	for(i = 0; i < nlines; i++)
		tkcmd(".t insert end '"+line);

	tkclient->onscreen(t, nil);
	tkclient->startinput(t, "kbd"::"ptr"::nil);

	restart();

	fontheight = int tkcmd(".t cget -actheight")/rows;
	fontwidth = int tkcmd(".t cget -actwidth")/columns;
	tkcmd("bind .t <Configure> {send cmd configure}");

	for(;;) alt {
	s := <-t.ctxt.kbd =>
		tk->keyboard(t, s);

	s := <-t.ctxt.ptr =>
		tk->pointer(t, *s);

	s := <-t.ctxt.ctl or
	s = <-t.wreq or
	s = <-wmctl =>
		tkclient->wmctl(t, s);

	s := <-ctrlc =>
		c := s[0];
		c -= 16r40;
		if(ewrite(tocmd, d := sys->aprint("%c", c), len d) != len d)
			seterror(sprint("write: %r"));
		else if(echo)
			drawc(c);

	e := <-escc =>
		e = "\u001b["+e;
		d := array of byte e;
		if(ewrite(tocmd, d, len d) != len d)
			seterror(sprint("write: %r"));

	cmd := <-cmdc =>
		if(dflag) warn(sprint("cmd: %q", cmd));
		case cmd {
		"paste" =>
			s := tkclient->snarfget();
			d := array of byte s;
			if(ewrite(tocmd, d, len d) != len d)
				seterror(sprint("write: %r"));
			else if(echo)
				for(i = 0; i < len s; i++)
					drawc(s[i]);
		"snarf" =>
			r := tkcmd(".t tag ranges sel");
			if(r == nil)
				break;
			s := tkcmd(".t get "+r);
			l := split(s, "\n");
			s = "";
			for(i = 0; i < len l; i++)
				s += droptl(l[i], " ")+"\n";
			tkclient->snarfput(s);

		"echo" =>
			echo = !echo;
			mecho := array[] of {"echo", "noecho"};
			tkcmd(sprint(".m entryconfigure %d -label '%s", Mecho, mecho[echo]));
		"escs" =>
			escs = !escs;
			mescs := array[] of {"esc", "noescs"};
			tkcmd(sprint(".m entryconfigure %d -label '%s", Mescs, mescs[escs]));
		"debug" =>
			dflag++;
		"nodebug" =>
			dflag = 0;
		"clear" =>
			st = nilstate;
			tkcmd(".t delete 1.0 end");
			line = mkline();
			for(i = 0; i < rows; i++)
				tkcmd(".t insert end '"+line);
			nlines = rows;
			tkcmd("update");

		"break" =>
			err := wf("/dev/termctl", array of byte "break");
			if(err != nil)
				warn("termctl break: "+err);

		"dim" =>
			err := wf("/dev/termctl", sys->aprint("dimensions %d %d", columns, rows));
			if(err != nil)
				warn("termctl dimensions: "+err);

		"configure" =>
			# temporarily disable binding, we're reconfiguring width/height
			tkcmd("bind .t <Configure> '");
			width := int tkcmd(".t cget -actwidth");
			height := int tkcmd(".t cget -actheight");
			ncols := width/fontwidth;
			nrows := height/fontheight;
			tkcmd(sprint(".t configure -width %d -height %d", width, height));
			tkcmd("bind .t <Configure> {send cmd configure}");

			err := wf("/dev/termctl", sys->aprint("dimensions %d %d", ncols, nrows));
			if(err != nil)
				warn("termctl dimensions: "+err);

			if(dflag) warn(sprint("new width %d, height %d, new cols %d, new rows %d", width, height, nrows, ncols));

			newrows := nrows-rows;
			newcols := ncols-columns;
			if(dflag) warn(sprint("newrows %d, newcols %d", newrows, newcols));
			if(newrows < 0) {
				tkcmd(sprint(".t delete {end linestart %dl} end", newrows));
			} else if(newrows > 0) {
				line = mkline();
				for(i = 0; i < newrows; i++)
					tkcmd(".t insert end '"+line);
			}
			nlines += newrows;
			rows = nrows;
			sety(st.y);

			if(newcols > 0) {
				space := mkspace(newcols);
				for(i = 0; i < rows; i++)
					tkcmd(sprint(".t insert {%d.0 lineend} '%s", tkline(i), space));
			} else if(newcols < 0) {
				for(i = 0; i < rows; i++)
					tkcmd(sprint(".t delete {%d.0 lineend %dc} {%d.0 lineend}", tkline(i), newcols, tkline(i)));
			}

			columns = ncols;
			setx(st.x);
			setcursor();
			scroll();
			tkcmd("update");

		"redial" =>
			restart();

		"x" =>
			warn("state:");
			warn(sprint("y %d, x %d, autowrap %d", st.y, st.x, st.autowrap));
			warn(sprint("bold %d underline %d, reverse %d, fg %d, bg %d", st.bold, st.underline, st.reverse, st.fg, st.bg));
			warn(sprint("echo %d, escs %d, columns %d, rows %d, nlines %d, cursor %d, .t end %q", echo, escs, columns, rows, nlines, cursor, tkcmd(".t index end")));
			setcursor();
			tkcmd("update");

		* =>
			warn(sprint("unknown command %#q", cmd));
		}
		tkcmd("focus .t");

	k := <-keyc =>
		s := "";
		s[0] = str->toint(k, 16).t0;
		d := array of byte s;
		if(ewrite(tocmd, d, len d) != len d)
			seterror(sprint("write: %r"));
		else if(echo)
			drawc(s[0]);

	(buf, err) := <-inc =>
		if(buf == nil && err == nil)
			seterror("eof");
		else if(err != nil)
			seterror("read: "+err);
		else {
			drawbuf(buf);
			setcursor();
			tkcmd("update");
		}
	}
}

restart()
{
	killgrp(pid());
	tkclient->settitle(t, str->quoted(command));
	tktitlecolor("blue");
	error = 0;
	tkcmd("update");

	inc = chan of (array of byte, string);
	cc = chan of int;
	syncc = chan of int;

	(fd0, fd1, fd2) := run(command);
	tocmd = fd0;
	spawn reader(fd1);
	spawn reader(fd2);
	spawn parser();
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

alert(s: string)
{
	tkclient->settitle(t, str->quoted(command)+": "+s);
}

seterror(s: string)
{
	if(!error) {
		alert(s);
		killgrp(pid());
		tktitlecolor("red");
		error = 1;
		tocmd = nil;
	} else if(dflag)
		warn("novt: "+s);
	tkcmd("update");
}

tktitlecolor(s: string)
{
	tkcmd(".Wm_t configure -bg "+s);
	tkcmd(".Wm_t.title configure -bg "+s);
	tkcmd(sprint("bind . <FocusIn> {.Wm_t configure -bg %s; .Wm_t.title configure -bg %s; update}", s, s));
}

setcursor()
{
	if(cursor)
		tkcmd(sprint(".t mark set insert %d.%d", tkline(st.y), st.x));
}

tkline(i: int): int
{
	return nlines-rows+1+i;
}

pos(): string
{
	return sprint("%d.%d", tkline(st.y), st.x);
}

lineclear(s, e, line: int, attrs: int)
{
	n := e-s;
	p := sprint("%d.%d", tkline(line), s);
	tkcmd(sprint(".t delete %s {%s +%dc}; .t insert %s '%s", p, p, n, p, mkspace(e-s)));
	if(attrs)
		tktags(sprint("%s {%s +%dc}", p, p, n));
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
	c := <-cc;
	if(sc)
		sync();
	return c;
}

sync()
{
	syncc <-= 1;
}

setx(v: int)
{
	st.x = minmax(0, v, columns-1);
}

sety(v: int)
{
	st.y = minmax(0, v, rows-1);
}

parser()
{
	for(;;) {
		onl := nlines;
		parse();
		if(onl != nlines) {
			setcursor();
			tkcmd("update");
		}
	}
}

parse()
{
	c := getc(0);

	case c {
	0 to 16r1f =>
		c0(c);

	16r7f =>
		if(dflag) warn("del");
	* =>
		if(dflag >= Chars) warn(sprint("c %c (%#x), at %d,%d", c, c, st.y, st.x));
		putchar(c);
		sync();
	}
}

c0names := array[] of {
"nul", "soh", "stx", "etx", "eot", "enq", "ack", "bel", "bs", "ht", "nl", "vt", "np", "cr", "so", "si",
"dle", "dc1", "dc2", "dc3", "dc4", "nak", "syn", "etb", "can", "em", "sub", "esc", "fs", "gs", "rs", "us",
};
c0(c: int)
{
	if(c != 16r1b && dflag >= Esc) warn(sprint("c0 %#q", c0names[c]));
	case c {
	16r00 =>	# filling, ignore
		;
	16r07 =>	# bell
		s := "bell!";
		if(bell)
			s = "     "+s;
		alert(s);
		bell = !bell;
	16r08 =>	# bs
		setx(st.x-1);
	16r09 =>	# ht, character tabulation, "\t"
		setx(((st.x+8)/8)*8);
	16r0a or	# lf, line feed, "\n"
	16r0b or	# vt, line tabulation
	16r0c =>	# ff, form feed, "\f"
		newline();
	16r0d =>	# cr, "\r", carriage return
		setx(0);
	16r1b =>
		if(escs) {
			sync();
			esc();
			return;
		} else {
			putchar(c);
		}
	* =>
		if(dflag) warn(sprint("unhandled c0 %#q", c0names[c]));
	}
	sync();
}

esc()
{
	c := getc(0);
	case c {
	16r40 to 16r5f =>
		c1(c);
	* =>
		escfs(c);
		sync();
	}
}

escfs(c: int)
{
	if(dflag >= Esc) warn(sprint("escfs %c (%#x)", c, c));
	case c {
	# "esc Fs" in ecma-48
	'c' =>	# ris, reset to initial state
		st = nilstate;
		tkcmd(sprint(".t delete {end -%dl} end", rows));
		line := mkline();
		for(i := 0; i < rows; i++)
			tkcmd(".t insert end '"+line);
		
	* =>
		if(dflag) warn(sprint("unknown independent control function, esc %c (esc %#x)", c, c));
	}
}

c1names := array[] of {
"", "", "bph", "nbh", "", "nel", "ssa", "esa", "hts", "htj", "vts", "pld", "plu", "r1", "ss2", "ss3",
"dcs", "pu1", "pu2", "sts", "cch", "mw", "spa", "epa", "sos", "", "sci", "csi", "st", "osc", "pm", "apc",
};
c1(c: int)
{
	if(c != '[' && dflag >= Esc) warn(sprint("c1 %#q", c1names[c-16r40]));
	case c {
	'_' or	# apc
	'P' or	# dcs
	']' or	# osc
	'^' or	# pm
	'X' =>	# sos
		# read to st: esc \
		sync();
		c = getc(1);
		for(;;) {
			if(c != 16r1b) {
				c = getc(1);
				continue;
			}
			c = getc(1);
			if(c == '\\')
				break;
		}
		return;

	'E' =>
		# nel, next line
		sety(st.y+1);
		setx(0);
	'M' =>
		# ri, reverse line feed
		sety(st.y-1);
	
	'[' =>
		csi();
		return;
	* =>
		if(dflag) warn(sprint("unhandled c1 %#q", c1names[c-16r40]));
	}
	sync();
}

csi()
{
	sync();
	c := getc(0);

	# read parameter
	p := "";
	while(c >= 16r30 && c <= 16r3f) {
		p[len p] = c;
		sync();
		c = getc(0);
	}

	# read intermediate
	i := "";
	while(c >= 16r20 && c <= 16r2f) {
		i[len i] = c;
		sync();
		c = getc(0);
	}

	# final command
	case c {
	16r40 to 16r7e =>
		control(p, i, c);
	* =>
		s := "";
		s[0] = c;
		if(dflag) warn(sprint("invalid csi, params %#q, intermediates %#q, final %#q", p, i, s));
	}
	sync();
}

control(p, im: string, c: int)
{
	im[len im] = c;
	if(dflag && dflag >= Esc) warn(sprint("control, %#q %#q", p, im));
	if(len im > 1) {
		if(dflag) warn(sprint("unrecognized csi, params %#q, command %#q", p, im));
		return;
	}
	a := split(p, ";");

	case c {
	# format effectors, ecma-48 8.2.4
	'`' =>	# hpa, character position absolute
		setx(value(1, a)-1);
	'j' =>	# hpb, character position backward
		setx(st.x-value(1, a));
	'a' =>	# hpr, character position forward
		setx(st.x+value(1, a));
	'f' =>	# hvp, character and line position
		sety(nvalue(1, a, 0)-1);
		setx(nvalue(1, a, 1)-1);
	'd' =>	# vpa, line position absolute
		sety(value(1, a)-1);
	'k' =>	# vpb, line position backward
		sety(st.y-value(1, a));
	'e' =>	# vpr, line position forward
		sety(st.y+value(1, a));

	# presentation control functions, ecma-48 8.2.5
	'm' =>	# sgr, select graphic rendition
		i := 0;
		if(len a == 0)
			a = array[] of {"0"};
		while(i < len a)
			case v := int a[i++] {
			0 =>
				st.bold = 0;
				st.underline = 0;
				st.reverse = 0;
				st.fg = Fg;
				st.bg = Bg;
			1 =>
				st.bold = 1;
			4 =>
				st.underline = 1;
			7 =>
				st.reverse = 1;
			10 =>
				; # primary font, we have no other
			22 =>
				st.bold = 0;
			24 =>
				st.underline = 0;
			27 =>
				st.reverse = 0;
			30 to 37 =>
				st.fg = v-30;
			39 =>
				st.fg = Fg;
			40 to 47 =>
				st.bg = v-40;
			49 =>
				st.bg = Bg;
			* =>
				if(dflag) warn(sprint("unknown mode %d, ignoring", v));
			}

	# editor functions, ecma-48 8.2.6
	'P' =>	# dch, delete character
		n := value(1, a);
		n = min(columns-st.x, n);
		s := sprint("%d.%d", tkline(st.y), st.x);
		tkcmd(sprint(".t delete %s {%s +%dc}", s, s, n));
		tkcmd(sprint(".t insert {%s lineend} '%s", s, mkspace(n)));
	'M' =>	# dl, delete line
		setx(0);
		n := value(1, a);
		n = min(rows-st.y, n);
		tkcmd(sprint(".t delete %d.0 %d.0", tkline(st.y), tkline(st.y+n)));
		for(i := 0; i < n; i++)
			tkcmd(sprint(".t insert end '%s", mkline()));
	'X' =>	# ech, erase character
		n := value(1, a);
		n = min(columns-st.x, n);
		l := tkline(st.y);
		tkcmd(sprint(".t delete %d.%d %d.%d; .t insert %d.%d '%s", l, st.x, l, st.x+n, l, st.x, mkspace(n)));
	'J' =>	# ed, erase in page
		case value(0, a) {
		0 =>
			lineclear(st.x, columns, st.y, 0);
			for(i := st.y+1; i < rows; i++)
				lineclear(0, columns, i, 0);
		1 =>
			for(i := 0; i < st.y; i++)
				lineclear(0, columns, i, 0);
			lineclear(0, st.x+1, st.y, 0);
		2 =>
			for(i := 0; i < rows; i++)
				lineclear(0, columns, i, 0);
		}
	'K' =>	# el, erase in line
		case value(0, a) {
		0 =>	lineclear(st.x, columns, st.y, 0);
		1 =>	lineclear(0, st.x+1, st.y, 0);
		2 =>	lineclear(0, columns, st.y, 0);
		}
	'@' =>	# ich, insert character
		n := value(1, a);
		n = min(columns-st.x, n);
		s := sprint("%d.%d", tkline(st.y), st.x);
		tkcmd(sprint(".t delete {%s lineend -%dc} {%s lineend}", s, n, s));
		tkcmd(sprint(".t insert %s '%s", s, mkspace(n)));

		# ecma-48 says we move to line home position, but programs don't expect this.
		# setx(0);

	'L' =>	# il, insert line
		setx(0);
		n := value(1, a);
		n = min(rows-st.y, n);
		tkcmd(sprint(".t delete %d.0 %d.0", tkline(rows-n), tkline(rows)));
		l := tkline(st.y);
		line := mkline();
		for(i := 0; i < n; i++)
			tkcmd(sprint(".t insert %d.0 '%s", l, line));

	# cursor control functions, emca-48 8.2.7
	'G' =>	# cha, cursor character absolute
		setx(value(1, a)-1);
	'E' =>	# cnl, cursor next line
		setx(0);
		sety(st.y+value(1, a));
	'F' =>	# cpl, cursor preceding line
		setx(0);
		sety(st.y-value(1, a));
	'D' =>	# cub, cursor left
		setx(st.x-value(1, a));
	'B' =>	# cud, cursor down
		sety(st.y+value(1, a));
	'C' =>	# cuf, cursor right
		setx(st.x+value(1, a));
	'H' =>	# cup, cursor position
		sety(nvalue(1, a, 0)-1);
		setx(nvalue(1, a, 1)-1);
	'A' =>	# cuu, cursor up
		sety(st.y-value(1, a));

	# mode setting, ecma-48 8.2.12
	'l' or	# rm, reset mode
	'h' =>	# sm, set mode
		v := c == 'h';
		for(i := 0; i < len a; i++)
			case int a[i] {
			* =>
				if(dflag) warn(sprint("mode %d, value %d, not implemented", int a[i], v));
			}

	# miscellaneous, ecma-48 8.2.14
	'b' =>	# rep, repeat
		n := value(1, a);
		n = min(n, columns-st.x);
		for(i := 0; i < n; i++)
			putchar(lastchar);

	* =>
		if(dflag) warn(sprint("unrecognized csi, params %#q, command %#q", p, im));
	}
}

split(s, sep: string): array of string
{
	l: list of string;
	while(s != nil) {
		e: string;
		(e, s) = str->splitstrl(s, sep);
		l = e::l;
		if(s != nil) {
			s = s[len sep:];
			if(s == nil)
				l = ""::l;
		}
	}
	return l2a(rev(l));
}

mkspace(n: int): string
{
	return string array[n] of {* => byte ' '};
}

mkline(): string
{
	d := array[columns+1] of {* => byte ' '};
	d[len d-1] = byte '\n';
	return string d;
}


tktag(tag, pos: string)
{
if(dflag >= Tkdraw) warn(sprint("tag %q, pos %q", tag, pos));
	tkcmd(sprint(".t tag add %s %s", tag, pos));
}

tktags(p: string)
{
	for(l := sys->tokenize(tkcmd(".t tag names "+p), " ").t1; l != nil; l = tl l)
		tkcmd(sprint(".t tag remove %s %s", hd l, p));

	if(st.underline)
		tktag("ul", p);
	if(!st.bold && st.fg == Fg && st.bg == Bg && !st.reverse)
		return;
	fg := string st.fg;
	bg := string st.bg;
	if(st.reverse) {
		tmp := fg;
		fg = bg;
		bg = tmp;
	}
	if(st.bold) {
		fg += "b";
		bg += "b";
	}
	tktag("f"+fg, p);
	tktag("b"+bg, p);
}

lastchar := 'x'; # need something
putchar(c: int)
{
	tkcmd(sprint(".t delete %s; .t insert %s '%c", pos(), pos(), c));
	tktags(pos());
	if(st.x == columns-1 && st.autowrap) {
		setx(0);
		newline();
	} else if(st.x < columns-1) {
		st.x++;
	}
	lastchar = c;
}

scroll()
{
	tkcmd(sprint(".t see end; .t see {end -%dl}", rows));
}

newline()
{
	if(st.y+1 >= rows) {
		tkcmd(sprint(".t insert %d.0 '%s", tkline(nlines), mkline()));
		nlines++;
		sety(rows-1);
		scroll();
	} else
		st.y++;
}

value(v: int, a: array of string): int
{
	return nvalue(v, a, 0);
}

isnum(s: string): int
{
	for(i := 0; i < len s; i++)
		if(s[i] < '0' || s[i] > '9')
			return 0;
	return s != nil;
}

nvalue(v: int, a: array of string, i: int): int
{
	if(i >= len a || !isnum(a[i]))
		return v;
	return int a[i];
}


tkcmd(s: string): string
{
	r := tk->cmd(t, s);
	if(r != nil && r[0] == '!' || dflag >= Tkdraw)
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
	sys->pctl(Sys->FORKFD|Sys->NEWPGRP, nil);
	sys->dup(fd0.fd, 0);
	sys->dup(fd1.fd, 1);
	sys->dup(fd2.fd, 2);
	sys->pctl(Sys->NEWFD, list of {0, 1, 2});
	fd0 = fd1 = fd2 = nil;
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

minmax(a, b, c: int): int
{
	return max(a, min(b, c));
}

wf(f: string, buf: array of byte): string
{
	fd := sys->open(f, Sys->OWRITE|Sys->OTRUNC);
	if(fd == nil || sys->write(fd, buf, len buf) != len buf)
		return sprint("write %q: %r", f);
	return nil;
}

pid(): int
{
	return sys->pctl(0, nil);
}

progctl(pid: int, s: string)
{
	f := sprint("/prog/%d/ctl", pid);
	fd := sys->open(f, Sys->OWRITE);
	sys->fprint(fd, "%s", s);
}

killgrp(pid: int)
{
	progctl(pid, "killgrp");
}

l2a[T](l: list of T): array of T
{
	a := array[len l] of T;
	i := 0;
	for(; l != nil; l = tl l)
		a[i++] = hd l;
	return a;
}

rev[T](l: list of T): list of T
{
	r: list of T;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

droptl(s, cl: string): string
{
	for(i := len s-1; i >= 0; i--)
		if(!str->in(s[i], cl))
			break;
	return s[:i+1];
}

min(a, b: int): int
{
	if(a < b) return a;
	return b;
}

max(a, b: int): int
{
	if(a > b) return a;
	return b;
}

stderr: ref Sys->FD;
warn(s: string)
{
	if(stderr == nil)
		stderr = sys->fildes(2);
	sys->fprint(stderr, "%s\n", s);
}

fail(s: string)
{
	warn(s);
	killgrp(pid());
	raise "fail:"+s;
}

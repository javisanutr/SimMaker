%%%%%%%%
%%% PRINT
%%%%%%%%

print_windows(F) :-
	%"Send to windows printer"::
	get(F, member, picture, P),
	get(@pce, convert, win_printer, class, _), !,
	(  	get_filename(F, Name)
	->  true
	;   Name = '<unknown job>'
	),
	new(Prt, win_printer(Name)),
	send(Prt, setup, P),
	send(Prt, open),
	send(F, report, progress, 'Printing ...'),

	get(P, bounding_box, area(X, Y, W, H)),
	get(@display, dots_per_inch, size(DX, DY)),
	InchW is W/DX,
	InchH is H/DY,

	get(Prt, size, size(PW0, PH0)),
	get(Prt, dots_per_inch, size(RX, RY)),
	MarX is RX,			% default 1 inch margins
	MarY is RY,
	PrInchW is (PW0-MarX*2)/RX,
	PrInchH is (PH0-MarY*2)/RY,

	send(Prt, map_mode, isotropic),
	(   InchW < PrInchW,
	    InchH < PrInchH		% it fits on the page
	->  OX is MarX,
		OY is PrInchH*RY - MarY - ((PrInchH - InchH)/2)*RY,
	    send(Prt, window, area(X, Y, DX, DY)),
	    send(Prt, viewport, area(OX, OY, RX, RY))
	;   Aspect is min(PrInchW/InchW, PrInchH/InchH),
	    ARX is integer(Aspect*RX),
	    ARY is integer(Aspect*RY),
	    send(Prt, window, area(X, Y, DX, DY)),
	    send(Prt, viewport, area(MarX, MarY, ARX, ARY))
	),
	send(Prt, draw_in, P?graphicals),
	initialize_print_text(F, Prt, Name, E, To, margin(LDPI,LDPI), BG),
	print_header(Prt, BG),
	send(Prt, next_page),
	print_pages(Prt, E, To, margin(LDPI,LDPI), BG, 2, _),
	send(Prt, close),
	free(Prt),
	send(F, report, done).

initialize_print_text(F, Prt, JobName, E, To, margin(LDPI,LDPI), BG) :-
	get(F, member, view, V),
	get(V, text_buffer, TB),
	new(E, editor(TB, 80)),
	send(E?image, elevation, @nil),
	send(E?image, pen, 0),
	get(TB, size, Size),
	From = 0,
	To = Size,
	get(E?area, width, Width),
	get(Prt, size, size(W,H)),
	get(Prt, dots_per_inch, size(DPIX,DPIY)),
	InchW is W/DPIX,
	InchH is H/DPIY,
	PageW is round(Width*(InchW/(InchW-2))), % 1 inch margin
	PageH is round(PageW*(H/W)),
	send(Prt, resolution, PageH),
	Height is round(PageH*((InchH-2)/InchH)),
	LDPI is round(PageH/InchH),
	send(E, do_set, 0, 0, Width, Height),

	new(BG, device),		% background
	(   JobName \== @default
	->  send(BG, display, new(T, text(JobName))),
	    send(BG, display, new(P, text)),
	    send(P, name, pageno),
	    get(T, height, TH),
	    BT is LDPI-TH/2,
	    Right is LDPI+Width,
	    send(T, position, point(LDPI, BT-TH)),
	    send(P, position, point(Right, BT-TH)),
	    send(BG, display, line(LDPI, BT, Right, BT)),
	    send(BG, attribute, page_right, Right)
	;   true
	),
	send(E, scroll_to, From, 1).

print_pages(Printer, Editor, End, Margin, BG, Page, Pages) :-
	get(Editor, image, Image),
	Margin = margin(MX, MY),
	(   get(BG, member, pageno, PageNoText)
	->  send(PageNoText, string, string('%s %d', page?label_name, Page)),
	    get(BG, page_right, Right),
	    send(PageNoText, x, Right-PageNoText?width)
	;   true
	),
	send(Printer, draw_in, BG),
	get(Image, end, EndImg),
	(   EndImg > End
	->  get(Image, start, Start),
	    get(Image, character_position, End, point(_,BaseY)),
	    send(Editor, do_set, height := BaseY),
	    send(Editor, scroll_to, Start, 1) % make sure it doesn't move
	;   true
	),
	send(Printer, draw_in, Image, point(MX, MY)),
	(   (   EndImg >= End
	    ;	get(Image, eof_in_window, @on)
	    )
	->  Pages = Page
	;   send(Printer, next_page),
	    send(Editor, scroll_to, EndImg, 1),
	    NextPage is Page+1,
	    print_pages(Printer, Editor, End, Margin, BG, NextPage, Pages)
	).
	
print_header(Printer, BG) :-
	(   get(BG, member, pageno, PageNoText)
	->  send(PageNoText, string, string('%s %d', page?label_name, 1)),
	    get(BG, page_right, Right),
	    send(PageNoText, x, Right-PageNoText?width)
	;   true
	),
	send(Printer, draw_in, BG).

postscript(F) :-
	% "Create PostScript in file"::
	get(F, member, picture, P),
	get(F, member, view, V),
	get_name_for_file(F, eps, Path),

	new(File, file(Path)),
	send(File, open, write),
	send(File, append, P?postscript),
	send(File, append, V?postscript),
	send(File, close),
	send(File, done),
	send(F, report, status, 'Saved PostScript in %s', Path).

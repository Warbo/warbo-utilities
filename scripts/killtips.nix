# Removes any visible tooltips on an X display. Tooltips are essentially small
# windows which get created and destroyed over and over. Sometimes these aren't
# handled properly, so they can hang around; since they appear on top of all
# other windows this can be really annoying. This script forcibly closes them.
# From http://shallowsky.com/blog/programming/killing-tooltips.html
{ python, wrap }:

wrap {
  name   = "killtips";
  paths  = [ (python.withPackages (p: [ p.xlib ])) ];
  script = ''
    #!/usr/bin/env python

    # Kill tooltips and similar override-redirect windows.
    # Copyright 2011 by Akkana Peck -- share and enjoy under the GPLv2 or later.

    # Empirically, tooltips have these properties:
    # - mapped
    # - override_redirect
    # - transient_for
    # wm_name may be the content of the tooltip or may be blank.
    # Sadly, menus have all those properties too, so this will also hide
    # any menus that happen to be open at the time.
    # But most well-behaved programs will remap their menus just fine
    # the next time you ask for them.

    # Can't import just Xlib and use Xlib.display, sigh
    from Xlib import display

    # Next line prints: Xlib.protocol.request.QueryExtension
    # There doesn't seem to be a way to suppress that, so just ignore it.
    dpy    = display.Display()
    screen = dpy.screen()
    root   = screen.root
    tree   = root.query_tree()

    for w in tree.children :
        att = w.get_attributes()
        if att.map_state and att.override_redirect and w.get_wm_transient_for():
            print "%d %d %35s %s" % (att.map_state, att.override_redirect,
                                     w.get_wm_transient_for(), w.get_wm_name())
            w.unmap()

    dpy.flush()
  '';
}

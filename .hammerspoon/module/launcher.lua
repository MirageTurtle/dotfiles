local applist = {
    {shortcut = 'H', appname = 'Emacs'},
    {shortcut = 'J', appname = 'Alacritty'},
    -- {shortcut = 'O', appname = 'Logseq'},
    {shortcut = 'M', appname = 'Eudic'},
    {shortcut = 'N', appname = "Firefox"},
}

-- Use Cmd+Alt+Ctrl+<shortcut> to launch or focus the app
-- Not using Cmd+Alt+<shortcut> because it is sometimes used by Emacs.
-- Not using Cmd+Shift+<shortcut> because it is often used by other apps.
hs.fnutils.each(applist, function (entry)
    hs.hotkey.bind({'cmd', 'ctrl', 'alt'}, entry.shortcut, entry.appname, function ()
        hs.application.launchOrFocus(entry.appname)
    end)
end)

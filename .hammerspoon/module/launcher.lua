local applist = {
    {shortcut = 'H', appname = 'Emacs'},
    {shortcut = 'J', appname = 'Alacritty'},
    -- {shortcut = 'O', appname = 'Logseq'},
    -- {shortcut = 'D', appname = 'Dictionary'},
    {shortcut = 'N', appname = "Chromium"},
}

-- Use Cmd+Alt+<shortcut> to launch or focus the app
-- Not useing Cmd+Shift+<shortcut> because it is often used by other purpose
-- (e.g., Cmd+Shift+F to find globally)
hs.fnutils.each(applist, function (entry)
    hs.hotkey.bind({'cmd', 'alt'}, entry.shortcut, entry.appname, function ()
        hs.application.launchOrFocus(entry.appname)
    end)
end)

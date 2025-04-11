local applist = {
    {shortcut = 'H', appname = 'Emacs'},
    {shortcut = 'J', appname = 'Alacritty'},
    -- {shortcut = 'O', appname = 'Logseq'},
    -- {shortcut = 'D', appname = 'Dictionary'},
    {shortcut = 'N', appname = "Chromium"},
}

-- Use Cmd+Shift+<shortcut> to launch or focus the app
-- Not useing Cmd+Alt+<shortcut> because it is sometimes used by Emacs.
hs.fnutils.each(applist, function (entry)
    hs.hotkey.bind({'cmd', 'shift'}, entry.shortcut, entry.appname, function ()
        hs.application.launchOrFocus(entry.appname)
    end)
end)

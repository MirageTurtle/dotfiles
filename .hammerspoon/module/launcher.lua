local applist = {
    {shortcut = 'F', appname = 'Emacs'},
    {shortcut = 'J', appname = 'Alacritty'},
    {shortcut = 'O', appname = 'Logseq'},
    {shortcut = 'D', appname = 'Dictionary'},
}

hs.fnutils.each(applist, function (entry)
    hs.hotkey.bind({'cmd', 'shift'}, entry.shortcut, entry.appname, function ()
        hs.application.launchOrFocus(entry.appname)
    end)
end)

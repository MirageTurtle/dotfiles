local applist = {
    {shortcut = 'H', appname = '/Applications/Emacs.app'},
    {shortcut = 'J', appname = 'Alacritty'},
    -- {shortcut = 'O', appname = 'Logseq'},
    {
        shortcut = 'M', appname = 'Eudic',
        precommand = '/usr/bin/defaults write com.eusoft.eudic.plist MAIN_TimesLeft 820711',
    },
    {shortcut = 'N', appname = "Firefox"},
    {shortcut = 'K', appname = "Ghostty"},
}

-- Use Cmd+Alt+Ctrl+<shortcut> to launch or focus the app
-- Not using Cmd+Alt+<shortcut> because it is sometimes used by Emacs.
-- Not using Cmd+Shift+<shortcut> because it is often used by other apps.
hs.fnutils.each(applist, function (entry)
    hs.hotkey.bind({'cmd', 'ctrl', 'alt'}, entry.shortcut, entry.appname, function ()
        if entry.precommand then
            hs.execute(entry.precommand)
        end
        hs.application.launchOrFocus(entry.appname)
    end)
end)

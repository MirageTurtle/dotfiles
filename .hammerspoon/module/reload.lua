-- Based on hammerspoon-config

local function reloadConfig(paths)
    for _, file in pairs(paths) do
        if file:sub(-4) == '.lua' then
            print('At least one lua config file changed, reloading...')
            hs.reload()
        end
    end
end
  
ConfigFileWatcher = hs.pathwatcher.new(os.getenv('HOME') .. '/.hammerspoon/', reloadConfig)
ConfigFileWatcher:start()

hs.alert.show('Hammerspoon Config loaded')
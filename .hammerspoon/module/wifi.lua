-- Based on hammerspoon-config
-- Wifi management
-- Manage others(e.g., window) based on wifi SSID and BSSID.

hs.location.start()

function ssidChangedCallback()
    -- I want to test network when I just open my Mac.
    ssid = hs.wifi.currentNetwork()
    if (ssid ~= nil) then
        print("ssid = " .. ssid)
        if (ssid == "NotFound") then
            hs.notify.new({title="Hammerspoon", informativeText="You are in office. Mute now.", autoWithdraw=true, withdrawAfter=3}):send()
            hs.audiodevice.defaultOutputDevice():setVolume(0)
            hs.audiodevice.defaultOutputDevice():setMuted(true)
        end
        if (ssid == "OpenWrt") then
        hs.notify.new({title="Hammerspoon", informativeText="You are at the doom, feel free.", autoWithdraw=true, withdrawAfter=3}):send()
        end
        if (ssid == "eduroam") then
            -- hs.notify.new({title="Hammerspoon", informativeText="You are in eduroam."}):send()
            -- TODO: Get BSSID
            bssid = hs.wifi.interfaceDetails()["bssid"]
            print(bssid)
            -- Eduroam BSSID near my doom: b8:e3:b1:4a:97:81
            if (bssid == "b8:e3:b1:4a:97:81") then
                hs.notify.new({title="Hammerspoon", informativeText="You are in doom, feel free.", autoWithdraw=true, withdrawAfter=3}):send()
            end
        end
    end
end

wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()

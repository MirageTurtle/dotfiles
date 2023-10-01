-- Based on hammerspoon-config
-- Wifi management
-- Manage others(e.g., window) based on wifi SSID.

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
    --     if (ssid == "eduroam") then
    --         -- TODO: Test network connection and if need, reconnect it.
    --         hs.notify.new({title="Hammerspoon", informativeText="You are in eduroam."}):send()
    --     end
    end
end

wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()

-- A majordomo, or butler/housekeeper.

-- post-wake commands
postWakeCommands = {
   "mtsshfs vlab:/home/vlab/Documents/logseq ~/Documents/sshfs-client/logseq",
   "mtsshfs vlab:/home/vlab/Documents/org ~/Documents/sshfs-client/org"
}

function postWake()
   for i, command in ipairs(postWakeCommands) do
      hs.execute(command)
   end
end

hs.caffeinate.watcher.new(
   function(event)
      if event == hs.caffeinate.watcher.systemDidWake then
	 postWake()
	 hs.notify.new({title="Majordomo", informativeText="System did wake"}):send()
      end
   end
):start()


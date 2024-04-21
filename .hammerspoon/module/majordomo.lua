-- A majordomo, or butler/housekeeper.
local logger = hs.logger.new('majordomo', 'info')

-- post-awake commands
postAwakeCommands = {
   "mtsshfs vlab:/home/vlab/Documents/logseq ~/Documents/sshfs-client/logseq",
   "mtsshfs vlab:/home/vlab/Documents/org ~/Documents/sshfs-client/org",
}

-- post-sleep commands
postSleepCommands = {
    -- "mysshfs-umount-all"
   "mysshfs-umount ~/Documents/sshfs-client/logseq",
   "mysshfs-umount ~/Documents/sshfs-client/org",
}

function postAwake()
   for i, command in ipairs(postAwakeCommands) do
      -- execute the command with user shell environment
      hs.execute(command, true)
   end
end

function postSleep()
   for i, command in ipairs(postSleepCommands) do
      hs.execute(command, true)
   end
end

-- https://gist.github.com/dropmeaword/cbf03189897733cbef6267dfbf36861d
function sleepWatcher(eventType)
   if (eventType == hs.caffeinate.watcher.systemWillSleep) then
      -- hs.alert.show("Going to sleep!")
      postSleep()
      -- hs.notify.new({title="Hammerspoon", informativeText="Post-sleep commands executed."}):send()
      -- just hammerspoon log, instead of notification
      logger.i("Post-sleep commands executed.")
   elseif (eventType == hs.caffeinate.watcher.systemDidWake) then
      -- hs.alert.show("Waking up!")
      postAwake()
      hs.notify.new({title="Hammerspoon", informativeText="Post-awake commands executed."}):send()
   end
end

hs.caffeinate.watcher.new(sleepWatcher):start()

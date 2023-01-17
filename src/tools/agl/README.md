# Angelize (agl) Cross-Platform Services Manager

Any modern server-side system needs a way to manage one or several services, hosted as Windows Services or POSIX Daemons. Even more with the today trend of MicroServices.

One common problem is to be able to avoid any service unavailability, i.e. watch and restart the services on failure. Another problem is to be able to deploy your solution on both Windows and POSIX servers, in a simple way. And nowaydays, even client applications need stand-alone services.

Just consider Angelize as a cross-platform [NSSM](https://nssm.cc) able to run one or several executables, and watch them. And it won't be flagged by some Windows anti-viruses as potentially dangerous as NSSM does, since it is widely used by some evil pieces of software. :) 

## Main Features

Angelize (aka `agl`) manages your Services:

- A main `agl`standard instance can be installed as a Windows Service, or a POSIX Daemon;
- This main service could be easily installed, started, stopped or uninstalled from the command line e.g. via `/install /start /stop /uninstall` commands on Windows - it is a regular `TSynDaemon`;
- This main service could also be run from a console, e.g. for testing, via `/run /verbose` commands;
- The `/start` command will start one or several sub-processes;
- Those sub-processes are started by level, i.e. as a sequence of rings of dependencies;
- Those sub-processes can be started by kind of Operating System, or even specific Linux distribution;
- You can optionally sleep some time or make HTTP/HTTPS requests during the start phase of each sub-process, to validate it actually started;
- Those sub-processes are either other Windows Services, or regular executables which are managed by the main service, or executables which are not managed by the main service but run by themselves;
- When a sub-process managed by the main service dies, it will be restarted automatically, with increasing pauses if it continues to fail;
- You can react to the integer exit code returned when a sub-process dies, e.g. to not retry the restart if the error seems fatal;
- If a sub-process died and did not restart (or was told to not restart), you can force the restart from the command line via `/retry`;
- The `/stop` command will try to first gracefully end the sub-process managed by the main service (using `WM_QUIT` or `SIGTERM`), then fallback to hard kill after some time (using `TerminateProcess` or `SIGKILL`);
- You can check the current sub-processes state from the command line via `/list`, or from a locally generated static HTML file, ready to be published via your local webserver;
- All settings for the main or sub services are stored as JSON UTF-8, for ease of tuning and installing (we don't touch the Windows registry);
- You can easily add a new setting file from the command line via `/new`;
- You can validate the settings files from the command line via `/settings`;
- Sub-processes can be additionally watched in the background by running some command-line requests, or some HTTP/HTTPS requests;
- All the process is logged in local text files, with detailed information and timing, ready for forensic;
- You can extend the parent `TSynAngelize` class with your own needs for your own project.

So, as you can see, not only a NSSM clone, but a whole new concept, which a much wider featureset.

## TODO

Without any priority order:
- Thread/CPU affinity;
- Impersonate user after starting, and POSIX chroot;
- Add some more monitoring information (CPU, RAM...) when running the sub-processes;
- More feedback during `/start /stop` command line process;
- Add a `/restart` command line switch as `/stop /start` shortcut;
- Test and debug the "Watch" feature.

Feedback is welcome!

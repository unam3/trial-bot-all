# trial-bot-all: Vkontakte and Telegram bot

## Installation and usage

To use bot one need to install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) and then build it from project directory:

```
stack build
```

Some preparations with VK need to be done: preface and part 1 of the [VK bots docs](https://vk.com/dev/bots_docs).

All configuration is done by `config.ini` file. Annotated example of which you may find in `_config.ini` file.

To start bot use next command:

```
stack exec trial-bot-all-exe
```

Use control and "c" keys to stop the bot.

## Known limitations

[Vk] Only messages that was received when bot is up and running will be processed.

[Vk] Keyboard in the answer to `/repeat` command will be accessible to all chat participants (VK limitation) and will be applied for the first one who use it.

Number of message repeats for all chats of the participant is the same.


## Project structure

One may find it out in old and simple fashion by `git ls-files | grep hs` command and browsing listed source files. Some usage of [the Handle Pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html) may be found.

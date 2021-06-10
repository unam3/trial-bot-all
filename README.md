# Telegram echo-bot: trial-bot

## Installation and Usage

To use `trial-bot` one need to install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) and then run in terminal from project directory:

```
stack build
```


All configuration is done by four parameters in trial-bot launch command (run from project directory):

```
stack exec trial-bot-exe token helpMsg repeatMsg echoRepeatNumberStr
```
where
- [token](https://core.telegram.org/bots/api#authorizing-your-bot)
- helpMsg — string, which bot will send to the `/help` message;
- repeatMsg — string, which bot will send to the `/repeat` message with currently set repeat number;
- echoRepeatNumberStr - number from 1 to 5 (inclusive) of repeats to one message.

Example of launch command in Linux environment:

```
stack exec trial-bot-exe "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11" "help msg" "repeat msg" 1
```


## Constraints

Repeat number will be same for the user in all chats with bot.

Only latest text message (from [updates](https://core.telegram.org/bots/api#getting-updates)) will be processed.

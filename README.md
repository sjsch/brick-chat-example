An example Brick program I made to demonstrate a few things.

![image](https://user-images.githubusercontent.com/33556084/127384466-09bc6e44-34b8-437d-a5d7-8b0f5e11e983.png)

To build this, use Nix:

```
nix-shell -p 'import (fetchGit { url = https://github.com/sjsch/brick-chat-example; }) {}'
```

Then, `chat-client <server> <nick>` and `chat-server` will be in your shell.

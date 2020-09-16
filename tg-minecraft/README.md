# Telegram system to start & stop minecraft

Parts:

- Microservice that listens to telegram messages
- SNS to send start, stop, rcon command messages
- SNS to receive start, stop events
- Systemd service on-device to start, stop the server

## Dev setup

1. Clone the repo
2. JRE 8 or above (built on OpenJDK 11)
3. ./gradlew :telegrambot:build

## Running

Run the Telegram bot with:

```bash
export TELEGRAM_BOT_TOKEN=...
./gradlew :telegrambot:run
```

## Deployment

Deploy it with:

```bash
./deploy.sh
```

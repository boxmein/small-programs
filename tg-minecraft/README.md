# Telegram system to start & stop minecraft

Parts:

- Microservice that listens to telegram messages: `telegrambot`
- SNS to send start, stop, rcon command messages
- SNS to receive start, stop events
- Systemd service on-device to start, stop the server: `service`

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

Test the Docker image:

```bash
./gradlew :telegrambot:docker
docker run -it --env TELEGRAM_BOT_TOKEN=$(pass Telegram/boxmein_bot) 242224638212.dkr.ecr.eu-north-1.amazonaws.com/boxmein-tgminecraft-bot
```

## Deployment

Authenticate to AWS ECR:

1. Set up Amazon ECR credential helper https://github.com/awslabs/amazon-ecr-credential-helper
2. Get your access keys etc

Deploy the Telegram bot with:

```bash
./gradlew :telegrambot:dockerPush
```

# Telegram system to start & stop minecraft

Parts:

- Microservice that listens to telegram messages: `telegrambot`
  - Microservice starts / stops AWS instance
  - After starting AWS instance, it fires an SNS message to start the Minecraft server
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
# Request this from @BotFather user on Telegram
export TELEGRAM_BOT_TOKEN=...
# Configure this to match your AWS setup
export AWS_DEFAULT_REGION=...
export AWS_ACCESS_KEY_ID=...
export AWS_SECRET_ACCESS_KEY=...
# The instance id of the VPS to start and stop
export SERVER_AWS_INSTANCE_ID=...
./gradlew :telegrambot:run
```

Test the Docker image:

```bash
./gradlew :telegrambot:docker
docker run -it \
  --env TELEGRAM_BOT_TOKEN=$(pass Telegram/boxmein_bot) \
  --env AWS_DEFAULT_REGION=eu-north-1 \
  --env AWS_ACCESS_KEY_ID=... \
  --env AWS_SECRET_ACCESS_KEY=... \
  --env SERVER_AWS_INSTANCE_ID=i-03c2fe6178924d185 \
  242224638212.dkr.ecr.eu-north-1.amazonaws.com/boxmein-tgminecraft-bot
```

## Deployment

Authenticate to AWS ECR:

1. Set up Amazon ECR credential helper https://github.com/awslabs/amazon-ecr-credential-helper
2. Get your access keys etc

Deploy the Telegram bot with:

```bash
./gradlew :telegrambot:dockerPush
```

# Telegram system to start & stop minecraft

Parts:

- Microservice that listens to telegram messages: `telegrambot`
  - Microservice starts / stops AWS instance
  - After starting AWS instance, it fires an SNS message to start the Minecraft server
- SNS to send start, stop, rcon command messages
- SNS to receive start, stop events
- Systemd service on-device: `service`
  - It starts the server when the service launches
  - It starts listening to messages from Pusher
  - It stops the server when the `"STOP"` message is received
  - It also provides rcon
  - It shuts down the server after the server gracefully stops

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
# Pusher is used as the notification backend bc it's free
export PUSHER_APP_ID=...
export PUSHER_APP_KEY=...
export PUSHER_APP_SECRET=...
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
  --env PUSHER_APP_ID=1078972 \
  --env PUSHER_APP_KEY=47db97bc033d8ab36649 \
  --env PUSHER_APP_SECRET=$(pass Pusher/app_secret_1078972) \
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

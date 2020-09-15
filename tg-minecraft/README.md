# Telegram system to start & stop minecraft

Parts:

- Microservice that listens to telegram messages
- SNS to send start, stop, rcon command messages
- SNS to receive start, stop events
- Systemd service on-device to start, stop the server

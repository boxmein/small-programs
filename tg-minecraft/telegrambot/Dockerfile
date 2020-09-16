FROM openjdk:12-jdk-alpine
ADD telegrambot-1.0.0.zip /opt/
WORKDIR /opt/
ENV APP_ENV=production
RUN cd /opt/ && \
    unzip /opt/telegrambot-1.0.0.zip && \
    rm /opt/telegrambot-1.0.0.zip
CMD /opt/telegrambot-1.0.0/bin/telegrambot

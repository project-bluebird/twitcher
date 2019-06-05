FROM node:8
COPY /deploy .
WORKDIR .

RUN npm install http-server -g

EXPOSE 8080

ENTRYPOINT http-server
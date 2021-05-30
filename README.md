# addki

I've been using [Anki](https://apps.ankiweb.net/) for about ten years
for language learning. For each foreign language word that I add to my
decks, I need to find its dictionary form/infinitive, part of speech,
and sometimes gender, pronunciation, and example sentences. I've been
doing this manually for multiple languages, and while I'm quite adept
at keyboard and mouse movements, it's a process that can surely be
automated.

I aim to use addki as a service (hosted somewhere, maybe even locally)
that will consume a list of foreign language words and automatically
collect the necessary information for each word to insert into my Anki
decks. According to the obligatory [xkcd comic](https://xkcd.com/1205/),
this project shouldn't take me more than six man-days of effort...

Note: I originally planned to implement addki using [Yesod](https://www.yesodweb.com/)
for learning, but gave up because implementing in Spring and Java was
a lot faster. Nevertheless, Yesod is used for [chesskell](https://github.com/darrenfoong/chesskell)
and I'm taking this opportunity to try out [Thymeleaf](https://www.thymeleaf.org/),
against the [SPA](https://en.wikipedia.org/wiki/Single-page_application)
movement.

## Building

```
gradle build
```

To build a container image:

```
gradle bootBuildImage
```

## Running

```
podman run --name postgres -e POSTGRES_PASSWORD=password -e POSTGRES_DB=addki -p 5432:5432 -d docker.io/postgres:13.3
gradle bootRun
```

Go to http://localhost:8080 or http://localhost:8080/swagger-ui/

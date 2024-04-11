
[In english](https://github.com/ciukstar/videre/blob/master/README.md)  

[En français](https://github.com/ciukstar/videre/blob/master/README.fr.md)  

[În română](https://github.com/ciukstar/videre/blob/master/README.ro.md)

## Videre
Прогрессивное веб-приложение для обмена мгновенными сообщениями

Приложение [Videre](https://videreru-2pg7fq7tgq-de.a.run.app) позволяет пользователям обмениваться мгновенными сообщениями через [WebSockets](https://developer.mozilla.org/ru/docs/Web/API/WebSockets_API), совершать аудио- и видеозвонки через [WebRTC](https://developer.mozilla.org/ru/docs/Web/API/WebRTC_API) и получать уведомления через [Web Push](https://developer.mozilla.org/ru/docs/Web/API/Push_API).

## Суперпользователь

* Имя пользователя  
  ```$YESOD_SUPERUSER_USERNAME```
* Пароль  
  ```$YESOD_SUPERUSER_PASSWORD```
  
Учетная запись суперпользователя определяется во время развертывания. Суперпользователь управляет другими пользователями и предоставляет или отзывает права администратора конкретным пользователям.

## Интеграция с внешними API

* Электронная почта: [Gmail API](https://developers.google.com/gmail/api/guides?hl=ru)  

  * Идентификатор клиента  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Секрет клиента  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

## Поисковая оптимизация

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```

## ER-диаграмма

![Диаграмма отношений сущностей](static/img/ERD_Videre.svg)

## Демо

[Нажмите здесь, чтобы увидеть демо](https://videreru-2pg7fq7tgq-de.a.run.app)

_* Нажмите на кнопку [![Demo user accounts](demo/button-demo-accounts.png)](https://videreru-2pg7fq7tgq-de.a.run.app/auth/login), чтобы получить список демонстрационных учетных записей пользователей_

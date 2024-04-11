
## Videre
Progressive Web App for Instant Messaging

Application [Videre](https://videre-2pg7fq7tgq-de.a.run.app) allows users to exchange instant messages via [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API), make audio and video calls via [WebRTC](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API), and be notified via [Web Push](https://developer.mozilla.org/en-US/docs/Web/API/Push_API).

## Superuser

* Username  
  ```$YESOD_SUPERUSER_USERNAME```
* Password  
  ```$YESOD_SUPERUSER_PASSWORD```
  
A superuser account is defined at deployment time. The superuser manages other users and grants or revokes administrator privileges to specific users.

## Integration with external APIs

* Email: [Gmail API](https://developers.google.com/gmail/api/guides)  

  * Client id  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Client secret  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

## Search Engine Optimization

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```

## ER Diagram

![Entity Relationship Diagram](static/img/ERD_Videre.svg)

## Demo

[Click here to see demo](https://videre-2pg7fq7tgq-de.a.run.app)

_* Click on the [![Demo user accounts](demo/button-demo-accounts.png)](https://videre-2pg7fq7tgq-de.a.run.app/auth/login) button to get a list of demo accounts_

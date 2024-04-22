
[In english](https://github.com/ciukstar/videre/blob/master/README.md)  

[En français](https://github.com/ciukstar/videre/blob/master/README.fr.md)  

[На русском](https://github.com/ciukstar/videre/blob/master/README.ru.md)

## Videre
Aplicație web progresivă pentru mesagerie instantanee

Aplicația [Videre](https://viderero-2pg7fq7tgq-de.a.run.app) permite utilizatorilor să schimbe mesaje instantanee prin [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API), să efectueze apeluri audio și video prin [WebRTC](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API) și să fie notificați prin [Web Push](https://developer.mozilla.org/en-US/docs/Web/API/Push_API).

## Utilizare
Pentru a se suna, utilizatorii trebuie să se [înregistreze](https://viderefr-2pg7fq7tgq-de.a.run.app/auth/login) în aplicație și să se adauge reciproc la lista de contacte dintre utilizatorii înregistrați.

Când adăugați un utilizator la lista de contacte, aplicația vă va cere permisiunea de a trimite notificări și de a vă abona la serviciul de notificare push.

## Superutilizator

* Nume de utilizator  
  ```$YESOD_SUPERUSER_USERNAME```
* Parola  
  ```$YESOD_SUPERUSER_PASSWORD```
  
Un cont de superutilizator este definit în momentul implementării. Superutilizatorul gestionează alți utilizatori și acordă sau revocă privilegii de administrator anumitor utilizatori.

## Integrare cu API-uri externe

* E-mail: [Gmail API](https://developers.google.com/gmail/api/guides)  

  * Id-ul clientului  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Secretul clientului  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

## Optimizare motor de căutare

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```

## Diagrama ER

![Diagrama entitate - relație](static/img/ERD_Videre.svg)

## Demo

[Click aici pentru a vedea demo](https://viderero-2pg7fq7tgq-de.a.run.app)

_* Faceți clic pe butonul [![Conturi de utilizator demonstrative](demo/button-demo-accounts.png)](https://viderero-2pg7fq7tgq-de.a.run.app/auth/login) pentru a obține o listă de conturi de utilizator demonstrativem_

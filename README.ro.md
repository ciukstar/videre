
[In english](https://github.com/ciukstar/videre/blob/master/README.md)  

[En français](https://github.com/ciukstar/videre/blob/master/README.fr.md)  

[На русском](https://github.com/ciukstar/videre/blob/master/README.ru.md)

## Videre
Mesagerie instantanee și apeluri video

## Prezentare generală
Aplicația [Videre](https://viderero-2pg7fq7tgq-de.a.run.app) permite utilizatorilor să schimbe mesaje instantanee prin [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API), să efectueze apeluri audio și video prin [WebRTC](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API) și să fie notificați prin [Web Push](https://developer.mozilla.org/en-US/docs/Web/API/Push_API).

## Utilizare
Pentru a se suna, utilizatorii trebuie să se [înregistreze](https://viderero-2pg7fq7tgq-de.a.run.app/auth/login) în aplicație și să se adauge reciproc la lista de contacte dintre utilizatorii înregistrați.

Când adăugați un utilizator la lista de contacte, aplicația vă va cere permisiunea de a trimite notificări și de a vă abona la serviciul de notificare push.

Aplicația folosește serviciul de notificare push al browserului pentru a notifica apelatul cu privire la un apel video sau audio primit.

La acceptarea apelului, o sesiune video/audio criptată peer-to-peer va începe între apelant și apelat, așa cum este descris de [protocolul WebRTC](https://www.w3.org/TR/webrtc/).

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

* Web Real-Time Communication: [WebRTC API](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API)

  * Configurare  
    ```$YESOD_RTC_PEER_CONNECTION_CONFIG```  
    Configurația poate fi furnizată ca o reprezentare textuală a unui obiect JSON, așa cum este descris în [RTCPeerConnection() constructor](https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection/RTCPeerConnection).  

    Folosit în principal pentru a specifica serverele STUN și TURN.

## Optimizare motor de căutare

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```
  
* [Yandex SEO](https://webmaster.yandex.com/welcome)

  ```$YESOD_YANDEX_VERIFICATION```

## Entităţile de bază

### Utilizator
Un utilizator nou se poate [înscrie](https://viderero-2pg7fq7tgq-de.a.run.app/auth/login) folosind un cont Google existent sau folosind o adresă de e-mail verificată. [API-ul Gmail](https://developers.google.com/gmail/api/guides) este folosit ca intermediar pentru a trimite linkuri de verificare către căsuța de e-mail a utilizatorului.

Un utilizator poate primi rolul de administrator de un superutilizator sau de un alt administrator. Numai utilizatorii cu rol de administrator au acces la datele administrative.

### Contact
Un contact este creat atunci când un utilizator adaugă un alt utilizator la lista de contacte. Dacă fiecare utilizator are unul pe celălalt în lista de contacte, atunci se pot suna.

### Abonament push
Un abonament push este creat atunci când un utilizator este adăugat la lista de contacte sau poate fi amânat ulterior.

Abonamentul poate fi verificat sau reînnoit din elementul de meniu „Vizualizare contact”.

[Notificările Web Push](https://developer.mozilla.org/en-US/docs/Web/API/Push_API) sunt folosite pentru a notifica un apelat despre un apel video/audio primit. De asemenea, este folosit pentru a încheia sesiunea video/audio.

### Entitatea „Apel”
Entitatea Apel reprezintă apelurile efectuate și primite către utilizator.

### Entitatea „Chat”
Entitatea Chat reprezintă mesajele schimbate între doi utilizatori. În mod implicit, mesajele de chat sunt schimbate și stocate folosind protocolul WebSockets. Mesageria peer-to-peer fără stocare intermediară este planificată pentru versiunile viitoare.

## Diagrama ER

![Diagrama entitate - relație](static/img/ERD_Videre.svg)

## Demo

[Click aici pentru a vedea demo](https://viderero-2pg7fq7tgq-de.a.run.app)

_* Faceți clic pe butonul [![Conturi de utilizator demonstrative](demo/button-demo-accounts.png)](https://viderero-2pg7fq7tgq-de.a.run.app/auth/login) pentru a obține o listă de conturi de utilizator demonstrativem_

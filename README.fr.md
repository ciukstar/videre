
[In english](https://github.com/ciukstar/videre/blob/master/README.md)  

[În română](https://github.com/ciukstar/videre/blob/master/README.ro.md)  

[На русском](https://github.com/ciukstar/videre/blob/master/README.ru.md)

## Videre
Application Web progressive pour la messagerie instantanée

L'application [Videre](https://viderefr-2pg7fq7tgq-de.a.run.app) permet aux utilisateurs d'échanger des messages instantanés via [WebSockets](https://developer.mozilla.org/fr/docs/Web/API/WebSockets_API), de passer des appels audio et vidéo via [WebRTC](https://developer.mozilla.org/fr/docs/Web/API/WebRTC_API) et d'être avertis via [Web Push](https://developer.mozilla.org/fr/docs/Web/API/Push_API).

## Usage
Pour s'appeler, les utilisateurs doivent [s'inscrire](https://viderefr-2pg7fq7tgq-de.a.run.app/auth/login) dans l'application et s'ajouter à leur liste de contacts parmi les utilisateurs enregistrés.

Lors de l'ajout d'un utilisateur à votre liste de contacts, l'application demandera l'autorisation d'envoyer des notifications et de s'abonner au service de notification push.

## Superutilisateur

* Nom d'utilisateur  
  ```$YESOD_SUPERUSER_USERNAME```
* Mot de passe  
  ```$YESOD_SUPERUSER_PASSWORD```
  
Un compte superutilisateur est défini au moment du déploiement. Le superutilisateur gère les autres utilisateurs et accorde ou révoque les privilèges d'administrateur à des utilisateurs spécifiques.

## Intégration avec des API externes

* E-mail: [Gmail API](https://developers.google.com/gmail/api/guides?hl=fr)  

  * Identifiant client  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Secret client  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

## Optimisation du moteur de recherche

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```

## Diagramme ER

![Diagramme entité-relation](static/img/ERD_Videre.svg)

## Démo

[Cliquez ici pour voir la démo](https://viderefr-2pg7fq7tgq-de.a.run.app)

_* Cliquez sur le bouton [![Comptes d'utilisateurs de démonstration](demo/button-demo-accounts.png)](https://viderefr-2pg7fq7tgq-de.a.run.app/auth/login) pour obtenir une liste des comptes d'utilisateurs de démonstration_

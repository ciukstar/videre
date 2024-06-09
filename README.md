
[En français](https://github.com/ciukstar/videre/blob/master/README.fr.md)  

[În română](https://github.com/ciukstar/videre/blob/master/README.ro.md)  

[На русском](https://github.com/ciukstar/videre/blob/master/README.ru.md)

## Videre
Instant Messaging and Video Calling

## Overview
Application [Videre](https://videre-2pg7fq7tgq-de.a.run.app) allows users to exchange instant messages via [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API), make audio and video calls via [WebRTC](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API), and be notified via [Web Push](https://developer.mozilla.org/en-US/docs/Web/API/Push_API).

## Usage
To call each other, users must [register](https://videre-2pg7fq7tgq-de.a.run.app/auth/login) in the application and add each other to their contact list from among registered users.

When adding a user to your contacts list, the app will ask for permission to send notifications and subscribe to the push notification service.

The app uses the browser's push notification service to notify the callee of an incoming video or audio call.

By accepting the call, a peer-to-peer encrypted video/audio session will start between the caller and the callee as described by the [WebRTC protocol](https://www.w3.org/TR/webrtc/).

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
	
* Web Real-Time Communication: [WebRTC API](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API)
  * Configuration  
    ```$YESOD_RTC_PEER_CONNECTION_CONFIG```  
	
    The configuration can be provided as a textual representation of a JSON object as described in [RTCPeerConnection() constructor](https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection/RTCPeerConnection).  

    Mainly used to specify STUN and TURN servers.

## Search Engine Optimization

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```
  
* [Yandex SEO](https://webmaster.yandex.com/welcome)

  ```$YESOD_YANDEX_VERIFICATION```

## Basic Entities

### User
A new user can [sign up](https://videre-2pg7fq7tgq-de.a.run.app/auth/login) using an existing Google account or using a verified email address. [Gmail API](https://developers.google.com/gmail/api/guides) is used as an intermediary to send verification links to the user's inbox.

A user can be grated the administrator role by a superuser or by another administrator. Only users with the administrator role have access to administrative data.

### Contact
A contact is created when a user adds another user to their contact list. If each user has each other in their contact list, then they can call each other.

### Push subscription
A push subscription is created when the user is added to the contact list, or it can be deferred until later.

The subscription can be checked or renewed from the “View contact” menu item.

[Web Push Notifications](https://developer.mozilla.org/en-US/docs/Web/API/Push_API) are used to notify a callee of an incoming video/audio call. It is also used to end the video/audio session.

### Call
The entity Call represents outgoing and incoming calls to the currently logged in user.

### Chat
The Chat entity represents messages exchanged between two users. By default, chat messages are exchanged and stored using the WebSockets protocol. Peer-to-peer messaging without intermediate storage is planned for future releases.

### Ringtone
...

## ER Diagram

![Entity Relationship Diagram](static/img/ERD_Videre.svg)

## Demo

[Click here to see demo](https://videre-2pg7fq7tgq-de.a.run.app)

_* Click on the [![Demo user accounts](demo/button-demo-accounts.png)](https://videre-2pg7fq7tgq-de.a.run.app/auth/login) button to get a list of demo accounts_

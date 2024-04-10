
self.onpush = function (e) {
  
  const message = e.data.json();

  e.waitUntil(
    self.clients.matchAll({ type: 'window' }).then(function (clients) {
      
      clients.forEach(function (client) { client.postMessage(message); });
      return message;
      
    }).then(function (message) {

      return self.registration.showNotification( message.title, {
        tag: message.messageType,
        renotify: true,
        image: message.senderPhoto,
        icon: message.icon,
        body: message.body
      } );
      
    })
  );
  
};


self.addEventListener('fetch', function (e) {

  if (e.request.method === 'GET' && e.request.url.includes('my/contacts/')) {

    e.respondWith(
      self.registration.pushManager.getSubscription().then(function (subscription) {
	
	if (!subscription) {
	  return fetch(e.request);
	} else {	  
	  let url = new URL(e.request.url);
	  url.searchParams.set('endpoint', subscription.endpoint);	  
	  return fetch(url);
	}
	
      })
    );

  }

});


self.onpush = function (e) {
  
  const message = e.data.json();

  e.waitUntil(
    self.clients.matchAll({ type: 'window', includeUncontrolled: true }).then((clients) => {
      
      const focused = clients.filter((client) => client.focused);

      focused.forEach((client) => { client.postMessage(message); });
      
      return [message, focused.length < 1];
      
    }).then(([message, notify]) => {
      
      return !notify
	? Promise.resolve()
	: self.registration.showNotification(message.title, {
          tag: message.messageType,
          renotify: true,
          icon: message.icon,
          body: message.body,
          image: message.senderPhoto,
	  actions: [{ action: 'accept',
		      title: 'Accept'
		    },
		    { action: 'decline',
		      title: 'Decline'
		    }]
	});
      
    })
  );
  
};

self.addEventListener('notificationclick', (e) => {
  e.notification.close();
});

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


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
  
  self.registration.pushManager.getSubscription().then(function (subscription) {
    console.log('subscription: ', subscription);    
  });
  
});

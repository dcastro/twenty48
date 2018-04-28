function LoginService() {
  
  init();

  function init() {
    gapi.load('client:auth2', () => {
      console.log('loaded');
      gapi.client.init({
        'apiKey': 'AIzaSyDEmI3F231zE25CR_BYqRHsSqL16atW8KI',
        'clientId': '587486773943-lb3cfne32q7t784ivehivj7rinjr4ds2.apps.googleusercontent.com',
        'scope': 'profile'
      }).then(() => {
        console.log('inited');
        const auth2 = gapi.auth2.getAuthInstance();

        if(auth2.isSignedIn.get())
          signedIn(auth2);
        else
          signedOut();

        auth2.isSignedIn.listen(isSignedIn => {
          if(isSignedIn)
            signedIn(auth2);
          else
            signedOut();
        });

        $('#sign-in').click(auth2.signIn);
        $('#sign-out').click(auth2.signOut);
      });
    });

    function signedIn(auth2) {
      const profile = auth2.currentUser.get().getBasicProfile()
      $("#avatar").attr('src', profile.getImageUrl());

      $("#signed-out").hide();
      $("#signed-in").show();
    }

    function signedOut() {
      $("#signed-in").hide();
      $("#signed-out").show();
    }
  }
}

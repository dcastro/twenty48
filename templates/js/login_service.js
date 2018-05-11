function LoginService(onSignIn) {
  
  init();

  this.userDetails = function() {
    return this.isSignedIn().then(function(signedIn) {

      if(!signedIn)
        return null;

      const profile = gapi.auth2.getAuthInstance().currentUser.get().getBasicProfile();

      return {
        email: profile.getEmail(),
        name: profile.getName()
      };
    });
  };

  this.isSignedIn = function() {
    return whenLoaded().then(function() {
      return gapi.auth2.getAuthInstance().isSignedIn.get();
    });
  };

  this.signIn = function() {
    return whenLoaded().then(function() {
       return gapi.auth2.getAuthInstance().signIn();
    });
  }

  function whenLoaded() {
    return new Promise(function(fulfilled, _) {
      gapi.load('client:auth2', fulfilled);
    });
  }

  function init() {
    gapi.load('client:auth2', function() {
      gapi.client.init({
        'apiKey': 'AIzaSyDEmI3F231zE25CR_BYqRHsSqL16atW8KI',
        'clientId': '587486773943-lb3cfne32q7t784ivehivj7rinjr4ds2.apps.googleusercontent.com',
        'scope': 'profile'
      }).then(function() {
        const auth2 = gapi.auth2.getAuthInstance();

        if(auth2.isSignedIn.get())
          signedIn(auth2);
        else
          signedOut();

        auth2.isSignedIn.listen(function(isSignedIn) {
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

      $("#login-area .loading").hide();
      $("#signed-out").hide();
      $("#signed-in").show();

      $("#save-score-button").hide();

      $("body").addClass("signed-in");

      onSignIn();
    }

    function signedOut() {
      $("#login-area .loading").hide();
      $("#signed-in").hide();
      $("#signed-out").show();

      $("body").removeClass("signed-in");
    }
  }
}

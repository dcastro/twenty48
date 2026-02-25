function LoginService(onSignIn) {
  const service = this;
  let gisInitialized = false;
  let currentProfile = null;

  // https://developers.google.com/identity/gsi/web/guides/display-google-one-tap#credential_response
  function parseJwt(token) {
    const base64Url = token.split('.')[1];
    const base64 = base64Url.replace(/-/g, '+').replace(/_/g, '/');
    const jsonPayload = decodeURIComponent(atob(base64).split('').map(function (c) {
      return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
    }).join(''));
    return JSON.parse(jsonPayload);
  }

  function signedIn(profile) {
    if (profile.picture) {
      $("#avatar").attr('src', profile.picture);
    }

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

  function ensureInitialized() {
    if (gisInitialized) {
      return;
    }

    google.accounts.id.initialize({
      client_id: '587486773943-lb3cfne32q7t784ivehivj7rinjr4ds2.apps.googleusercontent.com',
      callback: function (response) {
        const payload = parseJwt(response.credential);
        currentProfile = {
          email: payload.email || null,
          name: payload.name || null,
          picture: payload.picture || null
        };
        signedIn(currentProfile);
      }
    });

    gisInitialized = true;
  }

  this.userDetails = function () {
    return Promise.resolve(currentProfile);
  };

  this.isSignedIn = function () {
    return Promise.resolve(!!currentProfile);
  };

  this.signIn = function () {
    service.button_clicked();
  };

  this.signOut = function () {
    currentProfile = null;
    signedOut();
  };

  this.button_clicked = function () {
    ensureInitialized();
    google.accounts.id.prompt();
  };

  function init() {
    $("#sign-in").click(function () {
      service.button_clicked();
    });
    $("#sign-out").click(function () {
      service.signOut();
    });

    signedOut();
  }

  init();
}

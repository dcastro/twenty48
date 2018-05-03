// Wait till the browser is ready to render the game (avoids glitches)
window.requestAnimationFrame(function () {
  const loginSvc = new LoginService();
  new GameManager(4, KeyboardInputManager, HTMLActuator, LocalStorageManager, loginSvc);
});

// Wait till the browser is ready to render the game (avoids glitches)
window.requestAnimationFrame(function () {
  const login = new LoginService();
  new GameManager(4, KeyboardInputManager, HTMLActuator, LocalStorageManager);
});

function ScoreService(login) {

  this.saveScore = function(score) {
    const details = login.userDetails();

    if (details !== undefined) {
      details.score = score;
      $.post('/score', JSON.stringify(details));
    } else {
      console.error("Can't save scores unless logged in");
    }
  }
}
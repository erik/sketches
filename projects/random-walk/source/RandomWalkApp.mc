import Toybox.Application;
import Toybox.Lang;
import Toybox.WatchUi;

class RandomWalkApp extends Application.AppBase {

  public function initialize() {
    AppBase.initialize();
  }

  public function onStart(state as Dictionary?) as Void {}
  public function onStop(state as Dictionary?) as Void {}

  //! Return the initial view for the app
  //! @return Array Pair [View, Delegate]
  public function getInitialView() as Array<Views or InputDelegates>? {
    var view = new $.PromptView();
    return [
      view,
      new $.PromptDelegate(view)
    ] as Array<Views or InputDelegates>;
  }

}

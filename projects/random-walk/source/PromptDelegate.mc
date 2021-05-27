import Toybox.Lang;
import Toybox.WatchUi;

//! Handle input on initial view
class PromptDelegate extends WatchUi.BehaviorDelegate {

  private var _view as SensorHistoryBaseView;

  //! Constructor
  public function initialize(view as SensorHistoryBaseView) {
    BehaviorDelegate.initialize();

    _view = view;
  }

  public function onNextPage() as Boolean {
    _view.nextPrompt();
    WatchUi.requestUpdate();
    return true;
  }

  public function onPreviousPage() as Boolean {
    WatchUi.requestUpdate();
    return true;
  }
}

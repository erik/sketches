import Toybox.Graphics;
import Toybox.WatchUi;

//! Initial view
class PromptView extends WatchUi.View {
  private var _promptIndex as Number = 0;
  private var _prompts as Array<String> = [
    "LEFT @ Stop Sign",
    "RIGHT @ Fire Hydrant",
    "Go Uphill",
    "Follow Blue Cars",
  ];

  //! Constructor
  public function initialize() {
    View.initialize();
  }

  //! Update the view
  //! @param dc Device context
  public function onUpdate(dc as Dc) as Void {
    dc.setColor(Graphics.COLOR_BLACK, Graphics.COLOR_WHITE);
    dc.clear();

    dc.drawText(
      dc.getWidth() / 2,
      dc.getHeight() / 2,
      Graphics.FONT_MEDIUM,
      _prompts[_promptIndex],
      Graphics.TEXT_JUSTIFY_CENTER
    );
  }

  public function nextPrompt() as Void {
    _promptIndex = (_promptIndex + 1) % _prompts.size();
  }
}

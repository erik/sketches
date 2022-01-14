import Cocoa
import SwiftUI

// TODO: Can use this to override functionality, if needed.
class MainViewController: NSViewController {
  override func viewDidAppear() {
    super.viewDidAppear()
  }
}

class AppDelegate: NSObject, NSApplicationDelegate {
  var popover: NSPopover = NSPopover.init()
  var statusBar: StatusBarController?

  func applicationDidFinishLaunching(_ aNotification: Notification) {
    // Hide the dock icon
    NSApp.setActivationPolicy(.accessory)
    statusBar = StatusBarController.init(popover)

    let contentView = ContentView()
      .environmentObject(statusBar!)

    popover.behavior = .transient
    popover.animates = true
    popover.contentSize = NSSize(width: 360, height: 400)
    popover.contentViewController = NSHostingController(rootView: contentView)
  }

  func applicationWillTerminate(_ aNotification: Notification) {
    // Insert code here to tear down your application
  }
}

@main
struct JotApp: App {

  @NSApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
  var body: some Scene {
    Settings {
        /* TODO: Settings view */
    }
  }
}

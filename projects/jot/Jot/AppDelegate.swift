import Cocoa
import SwiftUI

// TODO: Can use this to override functionality, if needed.
class MainViewController: NSViewController {
    override func viewDidAppear() {
        super.viewDidAppear()
    }
}

class AppDelegate: NSObject, NSApplicationDelegate {
    var popover: NSPopover = .init()
    var statusBar: StatusBarController?

    let persistenceController = PersistenceController.shared

    func applicationDidFinishLaunching(_: Notification) {
        // Hide the dock icon
        NSApp.setActivationPolicy(.accessory)
        statusBar = StatusBarController(popover)

        let contentView = ContentView()
            .environmentObject(statusBar!)
            .environment(\.managedObjectContext, persistenceController.container.viewContext)

        // TODO: Call persistenceController.save() on scenePhase change.

        popover.behavior = .transient
        popover.animates = true
        popover.contentSize = NSSize(width: 360, height: 400)
        popover.contentViewController = NSHostingController(rootView: contentView)
    }

    func applicationWillTerminate(_: Notification) {
        persistenceController.save()
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

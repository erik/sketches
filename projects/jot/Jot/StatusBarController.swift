import AppKit
import Foundation
import SwiftUI

class StatusBarController: ObservableObject {
  private var statusItem: NSStatusItem!
  private var popover: NSPopover!

  init(_ popover: NSPopover) {
    self.popover = popover

    statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)

    if let button = self.statusItem.button {
      button.image = NSImage(named: "AppIcon")
      button.title = "📔"
      button.action = #selector(self.togglePopover)
      button.target = self
    }
    statusItem.isVisible = true
  }

  @objc func togglePopover(_ sender: AnyObject) {
      if self.popover.isShown {
          self.popover.performClose(sender)
      } else if let button = self.statusItem.button {
          self.popover.show(
            relativeTo: button.bounds,
            of: button,
            preferredEdge: NSRectEdge.minY
          )
      }
  }
}


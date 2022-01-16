import CoreData
import Foundation

struct PersistenceController {
    static let shared = PersistenceController()

    let container: NSPersistentContainer

    // A test configuration for SwiftUI previews. Populates example data.
    static var preview: PersistenceController = {
        let controller = PersistenceController(inMemory: true)

        let today = Calendar.current.startOfDay(for: Date())

        // Create some journal history.
        for i in 0 ..< 10 {
            let journal = JournalEntry(context: controller.container.viewContext)
            journal.date = Calendar.current.date(byAdding: DateComponents(day: -i), to: today)
            journal.note = "Today's thoughts: \(i)"

            // Create some TODO items
            for j in 0 ..< 3 {
                let todoItem = TodoItem(context: controller.container.viewContext)
                todoItem.journalEntry = journal
                todoItem.createdAt = Date()
                todoItem.isRemoved = false
                todoItem.isCompleted = j % 2 == 0
                todoItem.task = "TODO # \(j)"

                journal.addToTodoItems(todoItem)
            }
        }

        controller.save()
        return controller
    }()

    init(inMemory: Bool = false) {
        container = NSPersistentContainer(name: "Model")

        if inMemory {
            container.persistentStoreDescriptions.first?.url = URL(fileURLWithPath: "/dev/null")
        }

        container.loadPersistentStores { _, error in
            if let error = error {
                fatalError("Error: \(error.localizedDescription)")
            }
        }
    }

    func save() {
        let context = container.viewContext

        if context.hasChanges {
            do {
                print("save!")
                try context.save()
            } catch {
                print("Failed to save changes: \(error)")
            }
        }
    }
}

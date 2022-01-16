import CoreData
import Foundation

public extension JournalEntry {
    static func getOrCreateFor(date: Date, using: NSManagedObjectContext) -> JournalEntry {
        let fetchRequest = JournalEntry.fetchRequest()
        fetchRequest.predicate = NSPredicate(format: "%K == %@", #keyPath(JournalEntry.date), date as NSDate)

        do {
            if let journal = (try using.fetch(fetchRequest) as [JournalEntry]).first {
                return journal
            }
        } catch {
            print("failed to fetch JournalEntry \(error)")
        }

        let journal = JournalEntry(context: using)
        journal.date = Calendar.current.startOfDay(for: Date())
        using.insert(journal)

        return journal
    }

    func addTodo(of task: String, using moc: NSManagedObjectContext) {
        let todoItem = TodoItem(context: moc)

        moc.performAndWait {
            todoItem.journalEntry = self
            todoItem.createdAt = Date()
            todoItem.isRemoved = false
            todoItem.isCompleted = false
            todoItem.task = task

            self.addToTodoItems(todoItem)

            moc.insert(todoItem)
            try! moc.save()
        }
    }

    // TODO: This feels like a hack, is this necessary?
    var noteOrEmpty: String {
        get { note ?? "" }
        set { print("setting = \(newValue)"); note = newValue }
    }

    var todoItemsArray: [TodoItem] {
        let set = (todoItems?.set ?? []) as! Set<TodoItem>
        return set.sorted {
            ($0.isCompleted ? 1 : 0, $0.createdAt!) < ($1.isCompleted ? 1 : 0, $1.createdAt!)
        }
    }
}

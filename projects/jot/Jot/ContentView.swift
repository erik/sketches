import Combine
import SwiftUI

// Apparently not possible to modify this with SwiftUI yet.
// https://stackoverflow.com/questions/59813943/
extension NSTextField {
    override open var focusRingType: NSFocusRingType {
        get { .none }
        set {}
    }
}

struct PlanItemView: View {
    @Environment(\.managedObjectContext) var moc
    @ObservedObject var item: TodoItem

    func commitChange() {
        item.objectWillChange.send()
        try? moc.save()
    }

    var body: some View {
        HStack {
            Image(systemName: item.isCompleted ? "circle.fill" : "circle")
                .foregroundColor(.gray)
                .frame(width: 14, height: 14)

            Text(item.task!)
                .foregroundColor(item.isCompleted ? .secondary : .primary)
                .strikethrough(item.isCompleted, color: .secondary)
        }
        .contentShape(Rectangle()) // In order to make the whole thing clickable
        .onTapGesture { item.isCompleted = !item.isCompleted; commitChange() }
        .contextMenu { Button("Remove item", action: { item.isRemoved = true; commitChange() }) }
    }
}

extension NSTextView {
    override open var frame: CGRect {
        didSet {
            backgroundColor = .clear
            drawsBackground = true
        }
    }
}

struct DateHeader: View {
    let weekday: String!
    let monthday: String!
    let year: String!

    init(date: Date) {
        let fmt = DateFormatter()

        fmt.dateFormat = "EEEE"
        weekday = fmt.string(from: date)

        fmt.dateFormat = "MMMM dd"
        monthday = fmt.string(from: date)

        fmt.dateFormat = "yyyy"
        year = fmt.string(from: date)
    }

    var body: some View {
        HStack {
            Text(weekday)
                .font(.body.bold())
                + Text(" ")
                + Text(monthday)
                .font(.body.bold())
                .foregroundColor(.gray)
            Spacer()
        }
    }
}

class DebouncingExecutor: ObservableObject {
    private var timer: Timer?

    func afterDelay(of: TimeInterval, perform block: @escaping () -> Void) {
        timer?.invalidate()
        timer = Timer.scheduledTimer(
            withTimeInterval: of,
            repeats: false,
            block: { _ in block() }
        )
    }
}

struct JournalView: View {
    @Environment(\.managedObjectContext) var managedObjectContext

    private let debouncedSaveExecutor = DebouncingExecutor()
    private let placeholderText: String = "What's on your mind?"
    @State var notePersistenceDebouncer: Timer?

    @ObservedObject var journal: JournalEntry
    let isEditable: Bool

    @Namespace var planListBottomId
    @State var newPlanItem: String = ""

    func createNewTodo() {
        if !newPlanItem.isEmpty {
            journal.addTodo(
                of: newPlanItem,
                using: managedObjectContext
            )
        }

        newPlanItem = ""
    }

    var body: some View {
        DateHeader(date: journal.date!)
            .padding(.bottom, 5)

        ScrollViewReader { scrollViewReader in
            VStack(alignment: .leading, spacing: 5) {
                ForEach(journal.todoItemsArray, id: \.self) { item in
                    if !item.isRemoved {
                        PlanItemView(item: item)
                    }
                }

                if isEditable {
                    HStack {
                        Image(systemName: "plus.circle")
                            .foregroundColor(.gray)
                            .frame(width: 14, height: 14)
                            .onTapGesture {
                                createNewTodo()
                                withAnimation {
                                    scrollViewReader.scrollTo(planListBottomId)
                                }
                            }

                        TextField(
                            journal.todoItemsArray.isEmpty ? "Add plan..." : "Add another...",
                            text: $newPlanItem,
                            onCommit: {
                                createNewTodo()
                                withAnimation {
                                    scrollViewReader.scrollTo(planListBottomId)
                                }
                            }
                        )
                        .textFieldStyle(.plain)
                        .id(planListBottomId)
                    }
                }
            }
        }

        Spacer()
            .frame(minHeight: 15)

        if isEditable || !(journal.note ?? "").isEmpty {
            ZStack(alignment: .topLeading) {
                TextEditor(text: $journal.noteOrEmpty)
                    .onChange(of: journal.noteOrEmpty, perform: { _ in
                        debouncedSaveExecutor.afterDelay(of: 2.0, perform: {
                            managedObjectContext.perform {
                                try? managedObjectContext.save()
                            }
                        })
                    })
                    .disabled(!isEditable)
                    .foregroundColor(!isEditable ? .secondary : .primary)
                    .multilineTextAlignment(.leading)
                    .frame(height: 90)

                Text(placeholderText)
                    .foregroundColor(.secondary)
                    .disabled(!isEditable)
                    .allowsHitTesting(false)
                    .padding(.leading, 5)
                    .opacity(journal.noteOrEmpty == "" ? 1 : 0)
            }
            .padding()
            .background(Color(NSColor.textBackgroundColor).opacity(0.5))
            .cornerRadius(10)
            .font(.body)
        }
    }
}

struct JournalListView: View {
    @ObservedObject var currentJournal: JournalEntry
    @FetchRequest var previousJournals: FetchedResults<JournalEntry>

    // TODO: Would this break when the day rolls over?
    init(_ currentDate: Date, _ managedObjectContext: NSManagedObjectContext) {
        currentJournal = JournalEntry.getOrCreateFor(
            date: currentDate,
            using: managedObjectContext
        )

        _previousJournals = FetchRequest<JournalEntry>(
            sortDescriptors: [NSSortDescriptor(keyPath: \JournalEntry.date, ascending: false)],
            predicate: NSPredicate(format: "%K != %@", #keyPath(JournalEntry.date), currentDate as NSDate)
        )
    }

    var body: some View {
        VStack(alignment: .leading) {
            JournalView(
                journal: currentJournal,
                isEditable: true
            )

            ForEach(previousJournals, id: \.self) { journal in
                JournalView(
                    journal: journal,
                    isEditable: false
                )
            }

            VStack(alignment: .center) {
                Text("That's all!")
                    .font(.body)
                    .foregroundColor(.secondary)
                    .padding()
                    .frame(maxWidth: .infinity)
            }
        }
    }
}

class ContentViewModel: ObservableObject {
    @Published var currentDate: Date = Calendar.current.startOfDay(for: Date())

    init() {
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(dateChanged),
            name: .NSCalendarDayChanged,
            object: nil
        )
    }

    @objc func dateChanged() {
        DispatchQueue.main.sync {
            currentDate = Calendar.current.startOfDay(for: Date())
        }
    }
}

struct ContentView: View {
    @ObservedObject var viewModel = ContentViewModel()
    @EnvironmentObject var statusBar: StatusBarController
    @Environment(\.managedObjectContext) var managedObjectContext

    var body: some View {
        // TODO: Kind of wacky nesting going on here.
        GeometryReader { _ in
            ScrollView(showsIndicators: true) {
                JournalListView(viewModel.currentDate, managedObjectContext)
                    .padding()
            }
            .background(Color(NSColor.windowBackgroundColor))
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            ContentView()
                .environment(\.colorScheme, .dark)
                .environment(\.managedObjectContext, PersistenceController.preview.container.viewContext)

            ContentView()
                .environment(\.colorScheme, .light)
                .environment(\.managedObjectContext, PersistenceController.preview.container.viewContext)
        }
    }
}

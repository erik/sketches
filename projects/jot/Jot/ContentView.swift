import SwiftUI

class PlanItem: ObservableObject {
    var id: Int
    @Published var task: String
    @Published var isCompleted: Bool
    @Published var isRemoved: Bool = false

    init(id: Int, task: String, isCompleted: Bool) {
        self.id = id
        self.task = task
        self.isCompleted = isCompleted
    }
}

// Apparently not possible to modify this with SwiftUI yet.
// https://stackoverflow.com/questions/59813943/
extension NSTextField {
    override open var focusRingType: NSFocusRingType {
        get { .none }
        set {}
    }
}

struct PlanItemView: View {
    @StateObject var item: PlanItem

    var body: some View {
        HStack {
            Image(systemName: item.isCompleted ? "circle.fill" : "circle")
                .foregroundColor(.gray)
                .frame(width: 14, height: 14)

            Text(item.task)
                .foregroundColor(item.isCompleted ? .secondary : .primary)
                .strikethrough(item.isCompleted, color: .secondary)
        }
        .contentShape(Rectangle()) // In order to make the whole thing clickable
        .onTapGesture { item.isCompleted = !item.isCompleted }
        // FIXME: doesn't work.
        .contextMenu { Button("Remove item", action: { item.isRemoved = true }) }
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

struct NoteView: View {
    let placeholderText: String = "What's on your mind?"

    @State var text: String
    var disabled: Bool = false

    var body: some View {
        ZStack(alignment: .topLeading) {
            // TODO: If we want nicer padding here, I think we need to wrap in a scrollview
            TextEditor(text: $text)
                .disabled(disabled)
                .foregroundColor(.primary)
                .multilineTextAlignment(.leading)
                .frame(height: 90)

            Text(placeholderText)
                .foregroundColor(.secondary)
                .allowsHitTesting(false)
                .padding(.leading, 5)
                .opacity(self.text == "" ? 1 : 0)
        }
        .padding()
        .background(Color(NSColor.textBackgroundColor).opacity(0.5))
        .cornerRadius(10)
        .font(.body)
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

class DailyPlanModel: ObservableObject {
    @Published var date: Date = .init()
    @Published var planItems: [PlanItem] = [
        PlanItem(id: 0, task: "Figure out the scope of Jot", isCompleted: false),
        PlanItem(id: 1, task: "Plan the tech stack", isCompleted: true),
        PlanItem(id: 2, task: "Design this application in Figma", isCompleted: true),
    ]
}

struct TodayView: View {
    let date: Date
    let isEditable: Bool

    @Namespace var planListBottomId

    @State var notesText: String = ""
    @State var newPlanItem: String = ""
    @StateObject var dailyPlan: DailyPlanModel = .init()

    var body: some View {
        DateHeader(date: date)
            .padding(.bottom, 5)

        ScrollViewReader { scrollViewReader in
            VStack(alignment: .leading, spacing: 5) {
                ForEach(dailyPlan.planItems, id: \.id) { item in
                    if !item.isRemoved {
                        PlanItemView(item: item)
                    }
                }

                if isEditable {
                    HStack {
                        Image(systemName: "plus.circle")
                            .foregroundColor(.gray)
                            .frame(width: 14, height: 14)

                        TextField(
                            dailyPlan.planItems.isEmpty ? "Add plan..." : "Add another...",
                            text: $newPlanItem,
                            onCommit: {
                                if !newPlanItem.isEmpty {
                                    dailyPlan.planItems.append(
                                        PlanItem(
                                            id: dailyPlan.planItems.count,
                                            task: newPlanItem,
                                            isCompleted: false
                                        )
                                    )
                                }

                                newPlanItem = ""
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

        if isEditable || !notesText.isEmpty {
            NoteView(
                text: notesText,
                disabled: !isEditable
            )
        }
    }
}

struct ContentView: View {
    @EnvironmentObject var statusBar: StatusBarController

    var body: some View {
        // TODO: Kind of wacky nesting going on here.
        GeometryReader { _ in
            ScrollView(showsIndicators: true) {
                VStack(alignment: .leading) {
                    TodayView(
                        date: Date(),
                        isEditable: true
                    )

                    // TODO: Historical view goes here.
                    ForEach(1 ... 5, id: \.self) { dayOffset in
                        TodayView(
                            date: Calendar.current.date(
                                byAdding: DateComponents(day: -dayOffset),
                                to: Date()
                            )!,
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
            .padding()
            .background(Color(NSColor.windowBackgroundColor))
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            ContentView()
                .environment(\.colorScheme, .dark)

            ContentView()
                .environment(\.colorScheme, .light)
        }
    }
}

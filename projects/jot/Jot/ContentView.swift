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
    open override var focusRingType: NSFocusRingType {
        get { .none }
        set { }
    }
}

struct PlanItemView: View {
    @StateObject var item: PlanItem
    
    var body: some View {
        HStack {
            Circle()
                .strokeBorder(item.isCompleted ? Color.secondary : .gray, lineWidth: 1.0)
                .background(Circle().foregroundColor(item.isCompleted ? .secondary : .clear))
                .frame(width: 14, height: 14)
                //.padding(.leading, 4)
            
            Text(item.task)
                .foregroundColor(item.isCompleted ? .secondary : .primary)
                .strikethrough(item.isCompleted, color: .secondary)
        }
        .onHover { isWithin in
            DispatchQueue.main.async {
                if isWithin { NSCursor.pointingHand.push() }
                else { NSCursor.pop() }
            }
        }
        .contentShape(Rectangle()) // In order to make the whole thing clickable
        .onTapGesture { item.isCompleted = !item.isCompleted }
        // FIXME: doesn't work.
        .contextMenu { Button("Remove item", action: { item.isRemoved = true }) }
    }
}

extension NSTextView {
    open override var frame: CGRect {
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

struct ContentView: View {
    let placeholderText: String = "What's on your mind?"
    
    @EnvironmentObject var statusBar: StatusBarController
    @Namespace var planListBottomId
    
    @State private var text: String = ""
    @State private var newPlanItem: String = ""
    @State private var planItems: Array<PlanItem> = [
        PlanItem(id: 0, task: "Figure out the scope of Jot", isCompleted: false),
        PlanItem(id: 1, task: "Plan the tech stack", isCompleted: true),
        PlanItem(id: 2, task: "Design this application in Figma", isCompleted: true),
    ]
    
    var body: some View {
        // lmao
        GeometryReader { geometryReader in
            ScrollView(showsIndicators: false) {
                VStack(alignment: .leading) {
                    ScrollView {
                        DateHeader(date: Date.init())

                        ScrollViewReader { scrollViewReader in
                            VStack(alignment: .leading) {
                                ForEach(planItems, id: \.id) { item in
                                    if !item.isRemoved {
                                        PlanItemView(item: item)
                                    }
                                }
                                
                                HStack {
                                    Circle()
                                        .stroke(.gray)
                                        .background(Circle().foregroundColor(.clear))
                                        .frame(width: 14, height: 14)

                                    TextField(
                                        planItems.isEmpty ? "Add plan..." : "Add another...",
                                        text: $newPlanItem,
                                        onCommit: {
                                            if !newPlanItem.isEmpty {
                                                self.planItems.append(
                                                    PlanItem(
                                                        id: planItems.count,
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

                        Spacer()
                            .frame(minHeight: 15)
                        
                        NoteView(text: text)
                    }
                }
                .frame(height: geometryReader.size.height, alignment: .topLeading)
                
                VStack {
                    Text("here")
                    // TODO: Historical view goes here.
                }
            }
            .frame(alignment: .topLeading)
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

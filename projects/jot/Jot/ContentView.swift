import SwiftUI

struct PlanItem {
    var id: Int
    var task: String
    var isCompleted: Bool
}

// Apparently not possible to modify this with SwiftUI yet.
// https://stackoverflow.com/questions/59813943/
extension NSTextField {
    open override var focusRingType: NSFocusRingType {
        get { .none }
        set { }
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
                VStack {
                    VStack(alignment: .leading) {
                        Text("Today's Plan")
                            .font(.title)
                        
                        ScrollViewReader { scrollViewReader in
                            ScrollView {
                                VStack(alignment: .leading) {
                                    ForEach(planItems, id: \.id) { item in
                                        HStack {
                                            Circle()
                                                .strokeBorder(item.isCompleted ? .green : .gray, lineWidth: 1.0)
                                                .background(
                                                    Circle().foregroundColor(item.isCompleted ? .green : .clear)
                                                )
                                                .frame(width: 14, height: 14)
                                                .padding(.leading, 4)
                                            
                                            Text(item.task)
                                        }
                                        .onHover { isWithin in
                                            DispatchQueue.main.async {
                                                if isWithin { NSCursor.pointingHand.push() }
                                                else { NSCursor.pop() }
                                            }
                                        }
                                        .contentShape(Rectangle()) // In order to make the whole thing clickable
                                        .onTapGesture {
                                            self.planItems[item.id].isCompleted = !self.planItems[item.id].isCompleted
                                        }
                                        .contextMenu {
                                            Button("Remove item", action: {
                                                // TODO: implement me!
                                            })
                                        }
                                    }
                                    
                                    HStack {
                                        Circle()
                                            .stroke(.gray)
                                            .frame(width: 14, height: 14)
                                            .padding(.leading, 4)
                                        
                                        TextField(
                                            "Add another...",
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
                        }
                        
                        Text("Today's Notes")
                            .font(.title)
                        
                        ZStack(alignment: .topLeading) {
                            
                            TextEditor(text: $text)
                                .cornerRadius(6.0)
                                .multilineTextAlignment(.leading)
                                .frame(minHeight: 1, maxHeight: .infinity, alignment: .leading)
                            
                            Text(placeholderText)
                                .foregroundColor(.gray)
                                .padding(.leading, 5)
                                .opacity(self.text == "" ? 1 : 0)
                        }
                        .font(.body)
                    }
                }
                .padding()
                .frame(height: geometryReader.size.height)
                
                VStack {
                    // TODO: Historical view goes here.
                }
            }
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

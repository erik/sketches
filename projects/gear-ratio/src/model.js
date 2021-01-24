export class Bike {
  constructor ({ id, displayName, totalDistance }) {
    this.id = id
    this.displayName = displayName
    this.totalDistance = totalDistance

    this.components = []
    this.links = []
  }

  attachComponent (component) {
    this.components.push(component)
  }

  getComponent (type) {
    return this.components.find(it => it.type === type)
  }

  addLink (link) {
    this.links = []
  }

  href () {
    return `https://strava.com/bikes/${this.id}`
  }
}

export class BikeComponent {
  constructor ({ id, bikeId, href, type, titleText, totalDistance }) {
    this.id = id
    this.bikeId = bikeId
    this.href = href
    this.type = type
    this.titleText = titleText
    this.totalDistance = totalDistance
  }
}

export class BikeLink {
  constructor ({ displayName, bikes, componentTypes }) {
    this.displayName = displayName
    this.bikes = bikes
    this.componentTypes = componentTypes
  }

  getLinkedBikeIds () {
    return this.bikes.map(it => it.id)
  }

  sumTotalDistance () {
    return this.bikes.reduce((bike, totalDistance) => totalDistance + bike.totalDistance, 0)
  }

  sumComponents () {
    const components = {}

    for (const bike of this.bikes) {
      for (const type of this.componentTypes) {
        const component = bike.getComponent(type)
        const dist = component ? component.totalDistance : 0

        components[type] = (components[type] || 0) + dist
      }
    }

    return Object.entries(components)
      .map(([type, totalDistance]) => BikeComponent({
        type,
        totalDistance,

        id: null,
        href: 'todo?',
        titleText: 'todo: sum sources'
      }))
  }
}

export class Shoe {
  constructor (id, displayName, totalDistance) {
    this.displayName = displayName
    this.totalDistance = totalDistance
  }

  href () {
    return `https://strava.com/shoes/${this.id}`
  }
}

export class GearCollection {
  constructor (bikes, bikeComponents, shoes) {
    this.bikes = bikes
    this.shoes = shoes
    this.bikeLinks = []

    for (const c of bikeComponents) {
      this.attachBikeComponent(c)
    }
  }

  attachBikeComponent (component) {
    const bike = this.bikes.find(it => it.id === component.bikeId)
    if (typeof bike === 'undefined') {
      console.error('Failed to attach component, cannot find bike', component)
      return
    }

    bike.attachComponent(component)
  }

  createLink ({ displayName, bikeIds, componentTypes }) {
    const bikes = this.bikes.filter(it => bikeIds.includes(it.id))
    const link = new BikeLink({ displayName, bikes, componentTypes })

    for (const bike of bikes) {
      bike.addLink(link)
    }
  }
}

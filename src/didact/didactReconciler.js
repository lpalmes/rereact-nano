let rootInstance = null

function render(element, container) {
  const prevInstance = rootInstance
  const nextInstance = reconcile(container, prevInstance, element)
  rootInstance = nextInstance
}

function reconcile(parentDom, instance, element) {
  if (instance == null) {
    // Create instance
    const newInstance = instantiate(element)
    parentDom.appendChild(newInstance.dom)
    return newInstance
  } else if (instance.element.type === element.type) {
    // Update instance
    updateDomProperties(instance.dom, instance.element.props, element.props)
    instance.childInstances = reconcileChildren(instance, element)
    instance.element = element
    return instance
  } else {
    // Replace instance
    const newInstance = instantiate(element)
    parentDom.replaceChild(newInstance.dom, instance.dom)
    return newInstance
  }
}

function reconcileChildren(instance, element) {
  const dom = instance.dom
  const childInstances = instance.childInstances
  const nextChildElements = element.props.children || []
  const newChildInstances = []
  const count = Math.max(childInstances.length, nextChildElements.length)
  for (let i = 0; i < count; i++) {
    const childInstance = childInstances[i]
    const childElement = nextChildElements[i]
    const newChildInstance = reconcile(dom, childInstance, childElement)
    newChildInstances.push(newChildInstance)
  }
  return newChildInstances
}

function instantiate(element) {
  const { type, props } = element

  // Create DOM element
  const isTextElement = type === 'TEXT ELEMENT'
  const dom = isTextElement
    ? document.createTextNode('')
    : document.createElement(type)

  // Add event listeners
  const isListener = name => name.startsWith('on')
  Object.keys(props)
    .filter(isListener)
    .forEach(name => {
      const eventType = name.toLowerCase().substring(2)
      dom.addEventListener(eventType, props[name])
    })

  // Set properties
  const isAttribute = name => !isListener(name) && name != 'children'
  Object.keys(props)
    .filter(isAttribute)
    .forEach(name => {
      dom[name] = props[name]
    })

  // Instantiate and append children
  const childElements = props.children || []
  const childInstances = childElements.map(instantiate)
  const childDoms = childInstances.map(childInstance => childInstance.dom)
  childDoms.forEach(childDom => dom.appendChild(childDom))

  const instance = { dom, element, childInstances }
  return instance
}

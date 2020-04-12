const { main } = require('./1.js')

// some modules require inputs, this one doesn't
const { arrSum, arrLength } = main()

console.log('sum', arrSum, 'length', arrLength)

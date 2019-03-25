module.exports = {
  extends: ["eslint:recommended"],
  plugins: ["react"],
  rules: {
    "react/jsx-uses-react": "error",
    "react/jsx-uses-vars": "error"
  },
  env: {
    node: true
  },
  parser: "babel-eslint",
  parserOptions: {
    ecmaVersion: 8 // optional, recommended 6+
  }
};

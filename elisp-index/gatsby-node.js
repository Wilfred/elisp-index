const path = require(`path`);

exports.createPages = ({ graphql, actions }) => {
  const { createPage } = actions;

  return graphql(
    `
      {
        allLispJson {
          edges {
            node {
              name
              functions {
                name
              }
            }
          }
        }
      }
    `
  ).then(result => {
    if (result.errors) {
      throw result.errors;
    }

    const files = result.data.allLispJson.edges.map(e => e.node);
    const fileNames = files.map(f => f.name);
    let funNameToFile = {};
    files.forEach(file => {
      file.functions.forEach(fun => {
        funNameToFile[fun.name] = file.name;
      });
    });

    const postTemplate = path.resolve(`src/templates/lisp-file.js`);
    files.map(f => {
      const name = f.name;
      createPage({
        path: `file/${name}`,
        component: postTemplate,
        context: {
          name: name,
          funNameToFile: funNameToFile
        }
      });
    });
  });
};

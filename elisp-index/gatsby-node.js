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
            }
          }
        }
      }
    `
  ).then(result => {
    if (result.errors) {
      throw result.errors;
    }

    const postTemplate = path.resolve(`src/templates/lisp-file.js`);
    result.data.allLispJson.edges.map(edge => {
      const name = edge.node.name;
      createPage({
        path: `file/${name}`,
        component: postTemplate
      });
    });
  });
};

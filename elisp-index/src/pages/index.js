import React from "react";
import { Link, graphql } from "gatsby";

import Layout from "../components/layout";
import LinkedCode from "../components/linkedcode";
import SEO from "../components/seo";

const IndexPage = ({ data }) => {
  let fileLinks = data.allLispJson.edges.map(e => {
    const url = "/file/" + e.node.name;
    return (
      <li>
        <Link to={url}>{e.node.name}</Link>
      </li>
    );
  });

  return (
    <Layout>
      <SEO title="Home" keywords={[`gatsby`, `application`, `react`]} />
      <ul>{fileLinks}</ul>
    </Layout>
  );
};

export default IndexPage;

export const query = graphql`
  {
    allLispJson(sort: { fields: name }) {
      edges {
        node {
          name
        }
      }
    }
  }
`;

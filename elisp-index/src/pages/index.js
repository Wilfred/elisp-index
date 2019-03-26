import React from "react";
import { graphql } from "gatsby";

import Layout from "../components/layout";
import LinkedCode from "../components/linkedcode";
import SEO from "../components/seo";

const IndexPage = ({ data }) => {
  return (
    <Layout>
      <SEO title="Home" keywords={[`gatsby`, `application`, `react`]} />
      <LinkedCode source={data.dataJson.source} calls={data.dataJson.calls} />
    </Layout>
  );
};

export default IndexPage;

export const query = graphql`
  {
    dataJson {
      source
      calls {
        start
        end
        name
      }
    }
  }
`;

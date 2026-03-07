// @ts-check

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "yoshi",
  tagline: "Composable validation for Scala 3",
  url: "https://hshn.github.io",
  baseUrl: "/yoshi/",
  onBrokenLinks: "throw",
  markdown: {
    hooks: {
      onBrokenMarkdownLinks: "warn",
    },
  },
  organizationName: "hshn",
  projectName: "yoshi",

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          routeBasePath: "/",
          sidebarPath: require.resolve("./sidebars.js"),
        },
        blog: false,
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: "yoshi",
        items: [
          {
            href: "https://github.com/hshn/yoshi",
            label: "GitHub",
            position: "right",
          },
        ],
      },
      prism: {
        theme: require("prism-react-renderer").themes.github,
        darkTheme: require("prism-react-renderer").themes.dracula,
        additionalLanguages: ["java", "scala"],
      },
    }),
};

module.exports = config;

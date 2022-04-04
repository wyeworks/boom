import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :example_app, ExampleAppWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "yhLrVTlGNoZH8MpSvKBt1Y51HeCijQzdo3UhVOv6w+RZSjDU2WXqvXX5l3FntgtM",
  server: true,
  debug_errors: true

# Print only warnings and errors during test
config :logger, backends: []

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

config :wallaby, driver: Wallaby.Chrome, chrome: [headless: false]

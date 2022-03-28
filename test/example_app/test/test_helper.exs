ExUnit.start()

Application.put_env(:wallaby, :base_url, ExampleAppWeb.Endpoint.url())
{:ok, _} = Application.ensure_all_started(:wallaby)

{application, vkontakte,
[{description, "vkontakte"},
 {vsn, "0.4"},
 {modules, [
	vkontakte,
	vkontakte_sup,
	vkontakte_request
  ]},
 {registered,[vkontakte,vkontakte_sup]},
 {applications, [kernel,stdlib]},
 {mod, {vkontakte,[]}},
 {env, [
 ]}
]}.


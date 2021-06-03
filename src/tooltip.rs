//! A tooltip widget which can be used to display informations
//! on text hover.
//!
//! The implementation is based off
//! https://github.com/linebender/druid/blob/master/druid/examples/sub_window.rs.

// use std::time::{Duration, Instant};

// use druid::{
//     commands::CLOSE_WINDOW,
//     text::RichText,
//     widget::{Controller, Label},
//     Env, Event, EventCtx, LifeCycle, LifeCycleCtx, Point, Size, TimerToken, Widget, WindowConfig,
//     WindowId, WindowLevel, WindowSizePolicy,
// };

// enum TooltipState {
//     Showing(WindowId),
//     Waiting {
//         last_move: Instant,
//         timer_expire: Instant,
//         token: TimerToken,
//         window_pos: Point,
//     },
//     Fresh,
// }
// struct TooltipController {
//     text: RichText,
//     state: TooltipState,
// }

// impl TooltipController {
//     pub fn new(text: RichText) -> Self {
//         TooltipController {
//             text,
//             state: TooltipState::Fresh,
//         }
//     }
// }

// impl<T, W: Widget<T>> Controller<T, W> for TooltipController {
//     fn event(&mut self, child: &mut W, ctx: &mut EventCtx, event: &Event, data: &mut T, env: &Env) {
//         let wait_duration = Duration::from_millis(500);
//         let resched_dur = Duration::from_millis(50);
//         let cursor_size = Size::new(15., 15.);
//         let now = Instant::now();
//         let new_state = match &self.state {
//             TooltipState::Fresh => match event {
//                 Event::MouseMove(me) if ctx.is_hot() => Some(TooltipState::Waiting {
//                     last_move: now,
//                     timer_expire: now + wait_duration,
//                     token: ctx.request_timer(wait_duration),
//                     window_pos: me.window_pos,
//                 }),
//                 _ => None,
//             },
//             TooltipState::Waiting {
//                 last_move,
//                 timer_expire,
//                 token,
//                 window_pos,
//             } => match event {
//                 Event::MouseMove(me) if ctx.is_hot() => {
//                     let (cur_token, cur_expire) = if *timer_expire - now < resched_dur {
//                         (ctx.request_timer(wait_duration), now + wait_duration)
//                     } else {
//                         (*token, *timer_expire)
//                     };
//                     Some(TooltipState::Waiting {
//                         last_move: now,
//                         timer_expire: cur_expire,
//                         token: cur_token,
//                         window_pos: me.window_pos,
//                     })
//                 }
//                 Event::Timer(tok) if tok == token => {
//                     let deadline = *last_move + wait_duration;
//                     ctx.set_handled();
//                     if deadline > now {
//                         let wait_for = deadline - now;
//                         Some(TooltipState::Waiting {
//                             last_move: *last_move,
//                             timer_expire: deadline,
//                             token: ctx.request_timer(wait_for),
//                             window_pos: *window_pos,
//                         })
//                     } else {
//                         let win_id = ctx.new_sub_window(
//                             WindowConfig::default()
//                                 .show_titlebar(false)
//                                 .window_size_policy(WindowSizePolicy::Content)
//                                 .set_level(WindowLevel::Tooltip)
//                                 .set_position(
//                                     ctx.window().get_position()
//                                         + window_pos.to_vec2()
//                                         + cursor_size.to_vec2(),
//                                 ),
//                             Label::<()>::new(self.text.clone()),
//                             (),
//                             env.clone(),
//                         );
//                         Some(TooltipState::Showing(win_id))
//                     }
//                 }
//                 _ => None,
//             },
//             TooltipState::Showing(win_id) => {
//                 match event {
//                     Event::MouseMove(me) if !ctx.is_hot() => {
//                         // TODO another timer on leaving
//                         ctx.submit_command(CLOSE_WINDOW.to(*win_id));
//                         Some(TooltipState::Waiting {
//                             last_move: now,
//                             timer_expire: now + wait_duration,
//                             token: ctx.request_timer(wait_duration),
//                             window_pos: me.window_pos,
//                         })
//                     }
//                     _ => None,
//                 }
//             }
//         };

//         if let Some(state) = new_state {
//             self.state = state;
//         }

//         if !ctx.is_handled() {
//             child.event(ctx, event, data, env);
//         }
//     }

//     fn lifecycle(
//         &mut self,
//         child: &mut W,
//         ctx: &mut LifeCycleCtx,
//         event: &LifeCycle,
//         data: &T,
//         env: &Env,
//     ) {
//         if let LifeCycle::HotChanged(false) = event {
//             if let TooltipState::Showing(win_id) = self.state {
//                 ctx.submit_command(CLOSE_WINDOW.to(win_id));
//             }
//             self.state = TooltipState::Fresh;
//         }
//         child.lifecycle(ctx, event, data, env)
//     }
// }
